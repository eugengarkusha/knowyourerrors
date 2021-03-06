package errorhandling.testApi

import scala.concurrent.Future
import errorhandling.coproduct.Coproduct._
import shapeless.syntax.inject._

object Api {

  //errors definitions:
  case class Err(msg: String, cause: Exception)
  type Err1 = String
  type Err2 = Int
  type Err3 = Short
  type Err4 = Double
  type Err5 = Byte


  //different functions returing errs:
  type aErrType = Err1 +: Err2 :+: Err3
  def methodA: Future[Either[aErrType, String]] = Future.successful(Left(404.inject[aErrType]))

  type syncMethodErrType = Err1
  def syncMethod: Either[syncMethodErrType, String] = Left("Err1")

  type syncMethod1ErrType = Err2 :+: Err4
  def syncMethod1: Either[syncMethod1ErrType, String] = Right("res")

  type bErrType = Err4 :+: Err2
  def methodB: Future[Either[bErrType, String]] = Future.successful(Right("b"))

  type simpleErrorType = Err5
  def methodWithSimpleError: Future[Either[simpleErrorType, String]] = Future.successful(Right("b"))

  type simpleErrListType = List[Err1]
  def methodWithSimpleErrList: Future[Either[simpleErrListType, String]] = Future.successful(Left(List("")))

  type sumErrListType = List[Err2 :+: Err1]
  def methodWithSumErrList: Future[Either[sumErrListType, String]] = Future.successful(Left(List(200.inject[Err2 :+: Err1])))

  type methodWithGenErrType = Err1 :+: Err
  def methodWithGenErr: Future[Either[methodWithGenErrType, String]] = Future.successful(Right("noerr"))

  //no-err guys:
  def noErrorAsyncMethod1: Future[Option[String]] = Future.successful(Some("c"))
  def noErrorAsyncMethod2: Future[String] = Future.successful("d")

  def pureFunction: Int = 1
}
