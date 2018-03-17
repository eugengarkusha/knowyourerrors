package errorhandling.coproduct

import java.io.FileNotFoundException

import org.scalatest.FunSuite
import org.scalatest.Matchers
import shapeless.{Inl, Inr, CNil}
import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import errorhandling.apidesigndemo.Api
import errorhandling.apidesigndemo.ExternalApi
import errorhandling.apidesigndemo.Api._
import _root_.coproduct.Coproduct._
import _root_.coproduct.ops._
import _root_.ops.ops._
import _root_.ops.EitherOps._
import errors.{Cause, GenErr, GenError}
import shapeless.syntax.inject._
import shapeless.ops.coproduct.Remove


import scala.util.Try

class CombinationsTest extends FunSuite with Matchers {


  case class UnexcpectedErr(msg: String, cause: Cause) extends GenError
  case class NoStorage(msg: String, cause: Cause) extends GenError
  case class WrongInput(msg: String, cause: Cause) extends GenError


  test("combination of identical error and some non error contexts") {
    val mt = for {
      a <- Api.methodA.mt
      d <- Api.noErrorAsyncMethod2.toRight
      a1 <- Api.methodA.mt
    } yield a1

    val r: Either[aErrType, String] = Await.result(mt.v, 1.second)
    r should be(Left(Inr(Inl(404))))
  }


  test("combination of synchronous error and non error contexts") {


    type commonErr = Api.syncMethodErrType +: Api.syncMethod1ErrType

    val r: Either[commonErr, String] = for {
      a <- Api.syncMethod.align[commonErr]
      ne = Api.noErrorMethod
      a1 <- Api.syncMethod1.align[commonErr]
    } yield a1 + ne + a

    r should be(Left(Inl("Err1")))
  }

  test("partial handling") {

    val partiallyHandled: FutureEitherMT[Err2 :+:Err3, String] = {
      Api.methodA.partiallyHandle { err =>
        err._case[Api.Err1](_.toString)
      }
    }

    Await.result(partiallyHandled.v, 1.second) should be (Left(Inl(404)))

  }

  test("full handling") {

    val fullyHandled = {
      Api.methodA.handle {
        _._case[Api.Err1](e => s"e1 was: $e, now: OK")
         ._case[Api.Err2](e => s"e2 was: $e, now: OK")
         ._case[Api.Err3](e => s"e3 was: $e, now: OK")
      }
    }

    Await.result(fullyHandled, 1.second) should be ("e2 was: 404, now: OK")

  }

  //todo: move this to opsTest
  test("caseAll, caseOthers, suspend"){
    type commonSuper = UnexcpectedErr +: WrongInput :+: GenError

    UnexcpectedErr("err", null).inject[commonSuper]
      ._caseAll[GenError](_.toString) should be("UnexcpectedErr(err,null)")


    val partiallyHandled: Either[Int, String] ={
      UnexcpectedErr("err", null).inject[Int +: commonSuper]
      ._caseAll[GenError](_.toString)
      .suspend
    }
    partiallyHandled should be(Right("UnexcpectedErr(err,null)"))


    val err: commonSuper = (NoStorage("sdad", null): GenError).inject[commonSuper]

    val r2: String = err._case[WrongInput](_.toString)._caseAll[GenError](_.toString)
    r2 should be ("NoStorage(sdad,null)")

    //handling only the supertype
    val r3: Either[UnexcpectedErr +: WrongInput +: CNil, String] = err._case[GenError](_.toString).suspend
    r3 should be (Right("NoStorage(sdad,null)"))
  }

  test("partial wrapping") {
    //works the same as full wrapping shown in all-in-one case

    type wrappedErr = GenErr :+: NoStorage

    def wrapGenErr(g: GenErr): wrappedErr = {
      g.cause match {
        case  Some(Right(e:FileNotFoundException)) => NoStorage("comment", e).inject[wrappedErr]
        case _                          => g.inject[wrappedErr]
      }

    }

    val commonErrType = Flatten[Api.methodWithGenErrType +: Api.bErrType +: wrappedErr]

    val partiallyWrapped: FutureEitherMT[commonErrType.Out, (String, String)] = {
      for {
        a <- Api.methodWithGenErr.mt.leftMap(_.flatMapI[GenErr](wrapGenErr).align[commonErrType.Out])
        b <- Api.methodB.align[commonErrType.Out]
      } yield a -> b
    }

    Await.result(partiallyWrapped.v, 1.second) should be(Right("noerr" -> "b"))

  }

  test("all-in-one: combination of different error and non-error contexts and handling errors") {

    type genErrQualified = NoStorage +: WrongInput :+: UnexcpectedErr

    //Fully qualify the general error with concrete errors(partial wrapping is done the same way)
    def qualifyGenErr(g: GenErr): genErrQualified = {
      g.cause match{
        case Some(Right(e:FileNotFoundException))       => NoStorage("comment", e).inject[genErrQualified]
        case Some(Right(e: IllegalArgumentException))   => WrongInput("comment", e).inject[genErrQualified]
        case other: Cause => UnexcpectedErr("comment", other).inject[genErrQualified]
      }
    }


    //combining:

    val commonErr = {
      //Combining all methods errors signatures(except methodWithGenErr whose errors are partially transformed (see genErrQualified))
       Flatten[genErrQualified +: Api.aErrType +: Api.bErrType +: Api.sumErrListType +: Api.simpleErrListType :+: Api.simpleErrorType]
    }

    val mt = for {
      a     <- Api.methodA.align[commonErr.Out]
      b     <- Api.methodB.align[commonErr.Out]
      gen   <- Api.methodWithGenErr.mt.leftMap(_.flatMapI[GenErr](qualifyGenErr).align[commonErr.Out])
      aa    <- Api.syncMethod.futMtA[commonErr.Out]
      ext   <- Try(ExternalApi.excThrowingMethod).wrapWith(t => UnexcpectedErr("extApi err", Some(t.right))).futMtA[commonErr.Out]
      sel   <- Api.methodWithSimpleErrList.align[commonErr.Out]
      sumEl <- Api.methodWithSumErrList.align[commonErr.Out]
      se    <- Api.methodWithSimpleError.align[commonErr.Out]
      c     <- Api.noErrorAsyncMethod1.toRight
      d     <- Api.noErrorAsyncMethod2.toRight
    } yield (a, b, c, d, sel, sumEl, se)

    //expanding fl.Out for the sake of test descriptiveness
    type errType = NoStorage +: WrongInput +: UnexcpectedErr +: String +: Short +: Double +: Int +: List[Int :+: String] +: List[String] :+: Byte
    implicitly[errType =:= commonErr.Out]
    val result: Either[errType, (String, String, Option[String], String, String, String, String)] = Await.result(mt.v, 1.second)

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
            ._case[Api.Err2].apply(i => s"err at line $i")
            ._case[Api.Err1](s => s"user said $s")
        }

        errs
          ._caseAll[GenError](e=> e.msg)
          ._case[Api.Err1].apply(s => s"error:$s")
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
b) align errors:        val aligned = packed.align[GenErr]                              // ready to be combined in sync context
c) put into async ctx:  aligned.futMt  // +: skipping step 'b' packed.futMtA[GenErr]    // ready to be combined in async context
alltogether: Try(apicall).asEither(Error("some msg", cause = _)).futMtA[GenErr]

2) same as 1 (same as 1, points b and c)

3)
3a)prepare for combining: apicall.mt
3b)align errors and prare for combining: apicall.align[GenErr]

4)prepare for combining: apicall.right


reactions:
1)handle
2)partially handle
3)partially/fully wrap

 */

