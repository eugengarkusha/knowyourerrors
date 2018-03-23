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
import errorhandling.utils.flattenDedupType
import errorhandling.syntax.all._
import org.scalatest.{FunSuite, Matchers}
import shapeless.syntax.inject._
import shapeless.{Inl, Inr}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.util.Try

class CombinationsExample extends FunSuite with Matchers {


  trait GenError
  case class UnexcpectedErr(msg: String) extends GenError
  case class NoStorage(msg: String)extends GenError
  case class WrongInput(msg: String)extends GenError


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
      EitherT(Api.methodA).partiallyHandle (_.recoverFrom[Api.Err1](_.toString))

    Await.result(partiallyHandled.value, 1.second) should be (Left(Inl(404)))
  }

  test("full handling") {

    val fullyHandled = {
      EitherT(Api.methodA).handle {
        _.recoverFrom[Api.Err1](e => s"e1 was: $e, now: OK")
         .recoverFrom[Api.Err2](e => s"e2 was: $e, now: OK")
         .recoverFrom[Api.Err3](e => s"e3 was: $e, now: OK")
      }
    }

    Await.result(fullyHandled, 1.second) should be ("e2 was: 404, now: OK")

  }


  test("partial wrapping") {
    //works the same as full wrapping shown in all-in-one case

    type partiallyResolvedErr = Err :+: NoStorage

    def partiallyResolve(g: Err): partiallyResolvedErr = {
      g.cause match {
        case  e: FileNotFoundException => NoStorage("msg").inject[partiallyResolvedErr]
        case _                          => g.inject[partiallyResolvedErr]
      }
    }

    val commonErrType = flattenDedupType[Api.methodWithGenErrType +: Api.bErrType +: partiallyResolvedErr]

    val partiallyWrapped: EitherT[Future, commonErrType.Out, (String, String)] = {
      for {
        a <- EitherT(Api.methodWithGenErr).leftMap(_.flatMapI[Err](partiallyResolve).embed[commonErrType.Out])
        b <- EitherT(Api.methodB).embed[commonErrType.Out]
      } yield a -> b
    }

    Await.result(partiallyWrapped.value, 1.second) should be(Right("noerr" -> "b"))
  }

  test("all-in-one: combination of different error and non-error contexts and handling errors") {

    type genErrResolved = NoStorage +: WrongInput :+: UnexcpectedErr

    //Fully resolve the general error with concrete errors(partial wrapping is done the same way)
    def resolveGenErr(g: Err): genErrResolved = {
      g.cause match{
        case e:FileNotFoundException       => NoStorage("msg").inject[genErrResolved]
        case e: IllegalArgumentException  => WrongInput("msg").inject[genErrResolved]
        case other => UnexcpectedErr("msg").inject[genErrResolved]
      }
    }


    val commonErr = {
      //Combining all methods errors signatures(except methodWithGenErr whose errors are partially transformed (see genErrResolved))
      flattenDedupType[genErrResolved +: Api.aErrType +: Api.bErrType +: Api.sumErrListType +: Api.simpleErrListType :+: Api.simpleErrorType]
    }

    val mt = for {
      a     <- EitherT(Api.methodA).embed[commonErr.Out]
      b     <-  EitherT(Api.methodB).embed[commonErr.Out]
      gen   <-  EitherT(Api.methodWithGenErr).leftMap(_.flatMapI[Err](resolveGenErr).embed[commonErr.Out])
      aa    <-  EitherT.fromEither(Api.syncMethod.embed[commonErr.Out])
      ext   <-  EitherT.fromEither(Try(JavaApi.excThrowingMethod).toEither.left.map(t => UnexcpectedErr(s"extApi err: $t")).embed[commonErr.Out])
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
            .recoverFrom[Api.Err2](i => s"err at line $i")
            .recoverFrom[Api.Err1](s => s"user said $s")
        }

        errs
          .recoverFromAll[GenError](_.toString)
          .recoverFrom[Api.Err1](s => s"error:$s")
          .recoverFrom[Api.Err2](c => s"code $c")
          .recoverFrom[List[Api.Err2 :+: Api.Err1]](_.foldLeft("")((agr, next) => agr + " : " + handleListErr(next)))
          .recoverFromAll[Any](a => "other error")
      }
    }

    r should be("code 404")

  }
}
