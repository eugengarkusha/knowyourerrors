package ops

import coproduct.ops.{LiftCp, MatchSyntax}

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}
import scala.util.control.NonFatal
import EitherOps.FutureEitherMT
import coproduct.Coproduct._
import coproduct._
import shapeless.{<:!<,=:!=, HList, CNil, Coproduct}
import shapeless.ops.coproduct.{Inject, Basis, Prepend}
import misc.boolOps._
import coproduct.ops._
import cats.syntax.either._
import errors._



object ops {

  trait Align[Err, Super <: Coproduct] extends (Err => Super)
  object Align{
    implicit def x[Err, Lerr<: Coproduct, Super<: Coproduct](implicit lcp: LiftCp.Aux[Err, Lerr], e: Basis[Super, Lerr]):Align[Err, Super]  =
      v => e.inverse(Right(lcp(v)))
  }


  //TODO: put executionContext to methods signatures and extend AnyVal
  implicit class FutureEitherOps[Err, Result](t: Future[Either[Err, Result]])(implicit ec: ExecutionContext) {

    def mt: FutureEitherMT[Err, Result] = FutureEitherMT(t)
    def align[E <: Coproduct](implicit c: Align[Err, E]): FutureEitherMT[E, Result] = mt.leftMap(c)
    def partiallyHandle[L, R <: Coproduct, O, RR >: Result](f: Err => MatchSyntax.Case[L +: R, Result])(
      implicit _if: IF.Aux[R =:= CNil, L, L +: R, O]
    ): FutureEitherMT[O, RR] = {
      FutureEitherMT(t.map(_.partiallyHandle(f)))
    }
    def handle[RR >: Result](f: Err => RR): Future[RR] = mt.getOrElse(f)

  }

  //TODO: put executionContext to methods signatures and extend AnyVal!!!
  implicit class FutureEitherMTOps[Err, Result](t: FutureEitherMT[Err, Result])(implicit ec: ExecutionContext) {
    def align[E <: Coproduct](implicit c: Align[Err, E]): FutureEitherMT[E, Result] = t.leftMap(c)
    def partiallyHandle[L, R <: Coproduct, O, RR >: Result](f: Err => MatchSyntax.Case[L +: R, RR])(
      implicit _if: IF.Aux[R =:= CNil, L, L +: R, O]
    ): FutureEitherMT[O, RR] = {
      FutureEitherMT(t.v.map(_.partiallyHandle(f)))
    }
    def handle[RR >: Result](f: Err => RR): Future[RR] = t.getOrElse(f)

    def ensureCP[E, LL<: Coproduct, EE](err: Result => E)(cond: Result => Boolean)(
      implicit liftCp: LiftCp.Aux[Err, LL], add: Add.Aux[LL, E, EE]
    ): FutureEitherMT[EE, Result] = {
      t.copy(t.v.map(_.ensureCP[E, LL, EE](err)(cond)(liftCp, add)))
    }

    def ensureCP[E, LL<: Coproduct, EE](err: => E)(cond: Result => Boolean)(
      implicit liftCp: LiftCp.Aux[Err, LL], add: Add.Aux[LL, E, EE]
    ): FutureEitherMT[EE, Result] = {
      ensureCP[E, LL, EE]((_: Result) => err)(cond)(liftCp, add)
    }

  }

  //TODO: put executionContext to methods signatures and extend AnyVal
  implicit class FutureOps1[T](t: Future[T])(implicit ec: ExecutionContext) {
    def mt[L, R](l: Throwable => L, r: T => R ): FutureEitherMT[L, R] = {
      FutureEitherMT(t.map(v => Right(r(v))).recover{case NonFatal(t) => Left(l(t))})
    }
    def toRight[L]: FutureEitherMT[L, T] = FutureEitherMT(t.map(Right(_)))
    def toLeft[R]: FutureEitherMT[T, R] = FutureEitherMT(t.map(Left(_)))
  }


  // Err must be inferred
  implicit class EitherOps[Err, Res](e: Either[Err, Res])(implicit ev: Err =:!= Nothing) {

    def futMt(implicit ec: ExecutionContext): FutureEitherMT[Err, Res] = FutureEitherMT(Future.successful(e))

    def futMtA[E<: Coproduct](implicit c: Align[Err, E], ec: ExecutionContext): FutureEitherMT[E, Res] = {
      FutureEitherMT(Future.successful(e.left.map(c)))
    }

    def align[E<: Coproduct](implicit c: Align[Err, E]): Either[E, Res] = e.left.map(c)

    def partiallyHandle[L, R <: Coproduct, O, RR >: Res](f: Err => MatchSyntax.Case[L +: R, RR])(
      implicit _if: IF.Aux[R =:= CNil, L, L +: R, O]
    ): Either[O, RR] = e.left.map(err => f(err).suspend).joinLeft


    def handle[RR >: Res](f: Err => RR): RR = e.valueOr(f)

    def or[Err1, ErrCP <: Coproduct, Err1CP <: Coproduct, O <: Coproduct](e1: Either[Err1, Res])(
      implicit le: LiftCp.Aux[Err, ErrCP], le1: LiftCp.Aux[Err1, Err1CP], m: Prepend.Aux[ErrCP, Err1CP, O]
    ): Either[O, Res] = {
      //avoiding the runtime repacking
      def cast(e: Either[Any, Res]) = e.asInstanceOf[Either[O, Res]]
      e.fold(
        err => e1.fold(
          err1 => Left(m(Right(le1(err1)))),
          _ => cast(e1)
        ),
        _ => cast(e)
      )
    }

    def joinCP[Err2, Res2, LC1<: Coproduct, LC2<: Coproduct, MO <: Coproduct](
      implicit ev: Res <:< Either[Err2, Res2], liftCp: LiftCp.Aux[Err, LC1], liftCp2: LiftCp.Aux[Err2, LC2], m: Prepend.Aux[LC1, LC2, MO]
    ): Either[MO, Res2] = {
      e.fold(
        l => Left(m(Left(liftCp(l)))),
        r => ev(r).left.map(l => m(Right(liftCp2(l))))
      )
    }

    def ensureCP[E, LL<: Coproduct, EE](ll: => E)(cond: Res => Boolean)(implicit liftCp: LiftCp.Aux[Err, LL], add: Add.Aux[LL, E, EE]): Either[EE, Res] = {
      ensureCP[E, LL, EE]((_: Res) => ll)(cond)(liftCp, add)
    }

    def ensureCP[E, LL<: Coproduct, EE](ll: Res => E)(cond: Res => Boolean)(implicit liftCp: LiftCp.Aux[Err, LL], add: Add.Aux[LL, E, EE]): Either[EE, Res] = {
      e match {
        case Left(_err) => Left(add.extend(liftCp(_err)))
        case x@Right(res) => if (cond(res)) x.asInstanceOf[Either[EE, Res]] else Left(add(ll(res)))
      }
    }


    def hasError[E](implicit extract: Extract[Err, E, Invariant]): Boolean = e.isLeft && extract(e.left.get).isRight

  }

  implicit class AnyOps[T](t: T)(implicit ev: T <:!< CNil) {
    def addTo[C](implicit m: Add[C, T]): m.Out = m.apply(t)
    def left[R]: Either[T, R] = Left(t)
    def right[L]: Either[L, T] = Right(t)
  }

  //for non-future contexts use Either.cond(cond, (), err)
  def ensure[E](err: => E)(cond: Boolean)(implicit e: ExecutionContext): FutureEitherMT[E, Unit] = {
    if(cond) FutureEitherMT.right(()) else FutureEitherMT.left(err)
  }


  //workaround for scalas strange bahaviour.Cannot throw exception from _case[T], compiler cannot find IF instance if Res type == Nothing
  def _throw[T <: Throwable](t: T): Null = throw t



}
