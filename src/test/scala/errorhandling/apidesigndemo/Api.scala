package errorhandling.apidesigndemo

import scala.concurrent.Future
import _root_.coproduct._
import _root_.coproduct.ops._
import _root_.coproduct.Coproduct._
import errors.GenErr

object Api {

  //errors definitions:
  //TODO: create a suitable classes/traits to facilitate creation of errors having cause chain
  type Err1 = String
  type Err2 = Int
  type Err3 = Short
  type Err4 = Double
  type Err5 = Byte

  //different functions returing err+:s:
  type aErrType = Err1 +: Err2 :+: Err3
  def methodA: Future[Either[aErrType, String]] = Future.successful(Left(Inject(404).to[aErrType]))

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
  def methodWithSumErrList: Future[Either[sumErrListType, String]] = Future.successful(Left(List(Inject(200).to[Err2 :+: Err1])))

  type methodWithGenErrType = Err1 :+: GenErr
  def methodWithGenErr: Future[Either[methodWithGenErrType, String]] = Future.successful(Right("noerr"))

  //no-err guys:
  def noErrorAsyncMethod1: Future[Option[String]] = Future.successful(Some("c"))
  def noErrorAsyncMethod2: Future[String] = Future.successful("d")

  def noErrorMethod: Int = 1
}
