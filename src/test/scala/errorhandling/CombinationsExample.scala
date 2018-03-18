package errorhandling

import java.io.FileNotFoundException

import cats.data.EitherT
import cats.data.EitherT._
import cats.instances.future._
import errorhandling.coproduct.Coproduct._
import errorhandling.coproduct.ops._
import syntax._
import errorhandling.testApi.Api._
import errorhandling.testApi.{Api, JavaApi}
import errors._
import errorhandling.utils.flattenDedupType
import org.scalatest.{FunSuite, Matchers}
import shapeless.syntax.inject._
import shapeless.{Inl, Inr}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.util.Try

class CombinationsExample extends FunSuite with Matchers {


  case class UnexcpectedErr(msg: String, cause: Cause) extends GenError
  case class NoStorage(msg: String, cause: Cause) extends GenError
  case class WrongInput(msg: String, cause: Cause) extends GenError


  test("combination of identical error and some non error contexts") {
    val mt = for {
      a <- EitherT(Api.methodA)
      d <- liftF(Api.noErrorAsyncMethod2)
      a1 <- EitherT(Api.methodA)
    } yield a1

    val r: Either[aErrType, String] = Await.result(mt.value, 1.second)
    r should be(Left(Inr(Inl(404))))
  }


  test("combination of synchronous error and non error contexts") {

    type commonErr = Api.syncMethodErrType +: Api.syncMethod1ErrType

    val r: Either[commonErr, String] = for {
      a <- Api.syncMethod.embed[commonErr]
      ne = Api.pureFunction
      a1 <- Api.syncMethod1.embed[commonErr]
    } yield a1 + ne + a

    r should be(Left(Inl("Err1")))
  }

  test("partial handling") {
    val partiallyHandled: EitherT[Future, Err2 :+:Err3, String] =
      EitherT(Api.methodA).partiallyHandle (_._case[Api.Err1](_.toString))

    Await.result(partiallyHandled.value, 1.second) should be (Left(Inl(404)))
  }

  test("full handling") {

    val fullyHandled = {
      EitherT(Api.methodA).handle {
        _._case[Api.Err1](e => s"e1 was: $e, now: OK")
         ._case[Api.Err2](e => s"e2 was: $e, now: OK")
         ._case[Api.Err3](e => s"e3 was: $e, now: OK")
      }
    }

    Await.result(fullyHandled, 1.second) should be ("e2 was: 404, now: OK")

  }



  test("partial wrapping") {
    //works the same as full wrapping shown in all-in-one case

    type partQalifiedErr = GenErr :+: NoStorage

    def partiallyResolve(g: GenErr): partQalifiedErr = {
      g.cause match {
        case  Some(Right(e:FileNotFoundException)) => NoStorage("comment", e).inject[partQalifiedErr]
        case _                          => g.inject[partQalifiedErr]
      }

    }

    val commonErrType = flattenDedupType[Api.methodWithGenErrType +: Api.bErrType +: partQalifiedErr]

    val partiallyWrapped: EitherT[Future, commonErrType.Out, (String, String)] = {
      for {
        a <- EitherT(Api.methodWithGenErr).leftMap(_.flatMapI[GenErr](partiallyResolve).embed[commonErrType.Out])
        b <- EitherT(Api.methodB).embed[commonErrType.Out]
      } yield a -> b
    }

    Await.result(partiallyWrapped.value, 1.second) should be(Right("noerr" -> "b"))

  }

  test("all-in-one: combination of different error and non-error contexts and handling errors") {

    type genErrResolved = NoStorage +: WrongInput :+: UnexcpectedErr

    //Fully qualify the general error with concrete errors(partial wrapping is done the same way)
    def resolveGenErr(g: GenErr): genErrResolved = {
      g.cause match{
        case Some(Right(e:FileNotFoundException))       => NoStorage("comment", e).inject[genErrResolved]
        case Some(Right(e: IllegalArgumentException))   => WrongInput("comment", e).inject[genErrResolved]
        case other: Cause => UnexcpectedErr("comment", other).inject[genErrResolved]
      }
    }


    //combining:

    val commonErr = {
      //Combining all methods errors signatures(except methodWithGenErr whose errors are partially transformed (see genErrResolved))
      flattenDedupType[genErrResolved +: Api.aErrType +: Api.bErrType +: Api.sumErrListType +: Api.simpleErrListType :+: Api.simpleErrorType]
    }

    val mt = for {
      a     <- EitherT(Api.methodA).embed[commonErr.Out]
      b     <-  EitherT(Api.methodB).embed[commonErr.Out]
      gen   <-  EitherT(Api.methodWithGenErr).leftMap(_.flatMapI[GenErr](resolveGenErr).embed[commonErr.Out])
      aa    <-  EitherT.fromEither(Api.syncMethod.embed[commonErr.Out])
      ext   <-  EitherT.fromEither(Try(JavaApi.excThrowingMethod).toEither.left.map(t => UnexcpectedErr("extApi err", Some(Right(t)))).embed[commonErr.Out])
      sel   <-  EitherT(Api.methodWithSimpleErrList).embed[commonErr.Out]
      sumEl <-  EitherT(Api.methodWithSumErrList).embed[commonErr.Out]
      se    <-  EitherT(Api.methodWithSimpleError).embed[commonErr.Out]
      c     <-  EitherT.liftF[Future, commonErr.Out, Option[String]](Api.noErrorAsyncMethod1)
      d     <-  EitherT.liftF[Future, commonErr.Out, String](Api.noErrorAsyncMethod2)
    } yield (a, b, c, d, sel, sumEl, se)

    //expanding fl.Out for the sake of test descriptiveness
    type errType = NoStorage +: WrongInput +: UnexcpectedErr +: String +: Short +: Double +: Int +: List[Int :+: String] +: List[String] :+: Byte
    implicitly[errType =:= commonErr.Out]
    val result: Either[errType, (String, String, Option[String], String, String, String, String)] = Await.result(mt.value, 1.second)

    result should be(Left(Inr(Inr(Inr(Inr(Inr(Inr(Inl(404)))))))))

    //handling:
    val r: String = {

      result.map { res =>
        ///lines
        ///of
        ///code
        res.toString()
      }
      .handle { errs =>

        def handleListErr(error: Api.Err2 :+: Api.Err1) = {
          error
            ._case[Api.Err2](i => s"err at line $i")
            ._case[Api.Err1](s => s"user said $s")
        }

        errs
          ._caseAll[GenError](e=> e.msg)
          ._case[Api.Err1](s => s"error:$s")
          ._case[Api.Err2](c => s"code $c")
          ._case[List[Api.Err2 :+: Api.Err1]](_.foldLeft("")((agr, next) => agr + " : " + handleListErr(next)))
          ._caseAll[Any](a => "other error")
      }

    }

    r should be("code 404")

  }


}

/*
error contexts:
1)external synchronous api call which we expect to throw exceptions
2)internal synchronous api with error context(Either[L,R])
3)internal asynchronous api with error context (Future[Either[L,R]]
4)internal/external asynchronous api with NO err context

1)
a) pack:                val packed = Try(apicall).asEither(ConcreteError("some msg", cause = _))
a1)pack(general error): val packed = Try(apicall).pack  /*or pack("optionalMessage")*/  //will pack exception in GenErr
b).embed errors:        val.embeded = packed.embed[GenErr]                              // ready to be combined in sync context
c) put into async ctx: .embeded.futMt  // +: skipping step 'b' packed.futMtA[GenErr]    // ready to be combined in async context
alltogether: Try(apicall).asEither(Error("some msg", cause = _)).futMtA[GenErr]

2) same as 1 (same as 1, points b and c)

3)
3a)prepare for combining: apicall.mt
3b.embed errors and prare for combining: apicall.embed[GenErr]

4)prepare for combining: apicall.right


reactions:
1)handle
2)partially handle
3)partially/fully wrap

 */

