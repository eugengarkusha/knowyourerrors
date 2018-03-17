package ops

import scala.concurrent.{ExecutionContext, Future}
import cats.syntax.either._

object EitherOps {

  //TODO: put executionContext to methods signatures and extend AnyVal
  case class FutureEitherMT[+L, +R](v: Future[Either[L, R]])(implicit ec: ExecutionContext) {
    import EitherOps.{FutureEitherMT => MT}
    def map[T](f: R => T): MT[L, T] = copy(v.map(_.map(f)))

    def leftMap[K](f: L => K): MT[K, R] = copy(v.map(_.left.map(f)))

    def flatMap[LL >: L, RR](f: R => MT[LL, RR]): MT[LL, RR] = copy {
      v.flatMap(_.fold(l => Future.successful(Left(l)), r => f(r).v))
    }

    def flatMapL[LL, RR >: R](f: L => MT[LL, RR]): MT[LL, RR] = copy {
      v.flatMap(_.fold(l => f(l).v, r => Future.successful(Right(r))))
    }

    def flatMapF[LL >: L, RR](f: R => Either[LL, RR]): MT[LL, RR] = copy(v.map(_.flatMap(f)))

    def foreach[T](f: R => T): Unit = v.foreach(_.map(f))

    def join[LL >: L, RR](implicit ev: R <:< Either[LL, RR]): MT[LL, RR] = copy(v.map(_.joinRight))

    def joinL[LL, RR >: R](implicit ev: L <:< Either[LL, RR]): MT[LL, RR] = copy(v.map(_.joinLeft))

    def getOrElse[RR >: R](f: L => RR): Future[RR] = v.map(_.valueOr(f))

    def transform[LL, RR](lf: L => LL, rf: R => RR): MT[LL, RR] = copy(v.map(_.bimap(lf, rf)))

    def merge[S >: R](implicit ev: L <:< S): Future[S] = v.map(_.fold(ev, identity))

    def ensure[LL >: L](err: R => LL)(cond: R => Boolean): MT[LL, R] = copy(v.map (e => e.ensureOr(err)(cond)))

    def ensure[LL >: L](err: => LL)(f: R => Boolean): MT[LL, R] = ensure((_: R) => err)(f)

    def fold[O](lf: L => O, rf: R => O): Future[O] = v.map(_.fold(lf, rf))
  }

  object FutureEitherMT {
    def left[L, R](l: L)(implicit e: ExecutionContext): FutureEitherMT[L, R] = FutureEitherMT(Future.successful(Left(l)))
    def right[L, R](r: R)(implicit e: ExecutionContext): FutureEitherMT[L, R] = FutureEitherMT(Future.successful(Right(r)))
  }

  //rethink the scalaz ban
  def traverseE[M[X] <: TraversableOnce[X], T, L, R](m: M[T])(f: T => Either[L, R]): Either[L, Vector[R]] = {
    m.foldLeft[Either[L, Vector[R]]](Right(Vector.empty))((agr, next) => agr.flatMap(v => f(next).map(x => v :+ x)))
  }

  def traverseMT[M[X] <: TraversableOnce[X], T, L, R](m: M[T])(f: T => FutureEitherMT[L, R])(implicit ec: ExecutionContext): FutureEitherMT[L, Vector[R]] = {
    m.foldLeft[FutureEitherMT[L, Vector[R]]](FutureEitherMT.right(Vector.empty))((_agr, next) =>
      for {
        agr <- _agr
        nextRes <- f(next)
      } yield agr :+ nextRes
    )
  }
  def traverseMT[T, L, R](m: Option[T])(f: T => FutureEitherMT[L, R])(implicit ec: ExecutionContext): FutureEitherMT[L, Option[R]] = {
    m.map(f(_).map(Some(_))).getOrElse(FutureEitherMT.right(Option.empty[R]))
  }
}

