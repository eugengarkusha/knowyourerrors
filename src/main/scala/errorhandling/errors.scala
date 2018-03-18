package errorhandling

import cats.syntax.either._

import org.apache.commons.lang3.exception.ExceptionUtils

object errors {

  type Cause = Option[Either[GenError, Throwable]]

  //TODO: consider the simpler errors encoding: 1)GenErr extends Throwable; 2)Cause = Option[Throwable]
  //+less code
  //+this will give us a stackTrace mgmt for free
  //-we will loose case class pretty stringification
  //-our internal errors will _be_ throwables instead of being convertible to them
  trait GenError {
    //if the child class has more parameters, then 'msg' field should reflect that (it may contain an enumeration of the parameters)
    def msg: String
    def cause: Cause
    def stackTrace: String = ExceptionUtils.getStackTrace(toThrowable)
    def _throw(msg: String = ""): Nothing = throw new RuntimeException(msg, toThrowable)

    //dropping trace elements for Thread.getStackTrace and 'init methods of a this trait and the inheriting class
    private val _stackTrace: Array[StackTraceElement] = Thread.currentThread().getStackTrace.drop(3)

    private lazy val toThrowable: Throwable = {
      val className = getClass.getCanonicalName
      val _msg = s"$className($msg)"
      //scalastyle:off
      val e = cause.fold(new Throwable(_msg, null))(throwableOrErr =>
        new Throwable(_msg, throwableOrErr.valueOr(_.toThrowable)))
      //scalastyle:on
      e.setStackTrace(_stackTrace)
      e
    }
  }

  object GenError {
    implicit def genErrToCause(e: GenError): Cause = Some(Left(e))
    implicit def throwableToCause(e: Throwable): Cause = Some(Right(e))
  }

  trait NoCauseError extends GenError { def cause: Cause = None }

  case class GenErr(msg: String, cause: Cause) extends GenError

  case class ValidationErr(msg: String, cause: Cause = None) extends GenError

}
