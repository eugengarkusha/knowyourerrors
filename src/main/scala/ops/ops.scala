package ops

import coproduct.ops.{Align, AndThen, LiftCp, MatchSyntax}

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}
import scala.util.control.NonFatal
import EitherOps.FutureEitherMT
import coproduct.Coproduct._
import coproduct._
import shapeless.{<:!<,=:!=, HList, CNil, Coproduct}
import shapeless.ops.coproduct.Inject
import misc.boolOps._
import coproduct.ops._
import cats.syntax.either._
import errors._



object ops {

  //  Raw example of what can be done:

  //TODO: put executionContext to methods signatures and extend AnyVal
  implicit class FutureEitherOps[Err, Result](t: Future[Either[Err, Result]])(implicit ec: ExecutionContext) {
    def mt: FutureEitherMT[Err, Result] = FutureEitherMT(t)
    def align[E](implicit c: AndThen.Aux[Err, LiftCp.Aux, Align, E]): FutureEitherMT[E, Result] = mt.leftMap(err => c.b(c.a(err)))
    def partiallyHandle[L, R <: Coproduct, O, RR >: Result](f: Err => MatchSyntax.Case[L +: R, Result])(
      implicit _if: IF.Aux[R =:= CNil, L, L +: R, O]
    ): FutureEitherMT[O, RR] = {
      FutureEitherMT(t.map(_.partiallyHandle(f)))
    }
    def handle[RR >: Result](f: Err => RR): Future[RR] = mt.getOrElse(f)

    //applies polyfunction to left side (errors)
    def adapt[F <: HList, O](f: => PolyFunc[F])(implicit m: UniformPolyMap.Aux[Err, F, O]): FutureEitherMT[O, Result] = mt.leftMap(f(_))
  }

  //TODO: put executionContext to methods signatures and extend AnyVal!!!
  implicit class FutureEitherMTOps[Err, Result](t: FutureEitherMT[Err, Result])(implicit ec: ExecutionContext) {
    def align[E](implicit c: AndThen.Aux[Err, LiftCp.Aux, Align, E]): FutureEitherMT[E, Result] = t.leftMap(err => c.b(c.a(err)))
    def partiallyHandle[L, R <: Coproduct, O, RR >: Result](f: Err => MatchSyntax.Case[L +: R, RR])(
      implicit _if: IF.Aux[R =:= CNil, L, L +: R, O]
    ): FutureEitherMT[O, RR] = {
      FutureEitherMT(t.v.map(_.partiallyHandle(f)))
    }
    def handle[RR >: Result](f: Err => RR): Future[RR] = t.getOrElse(f)

    def adapt[F <: HList, O](f: => PolyFunc[F])(implicit m: UniformPolyMap.Aux[Err, F, O]): FutureEitherMT[O, Result] = t.leftMap(f(_))

    def ensureCP[E, LL, EE](err: Result => E)(cond: Result => Boolean)(
      implicit liftCp: LiftCp.Aux[Err, LL], add: Add.Aux[LL, E, EE]
    ): FutureEitherMT[EE, Result] = {
      t.copy(t.v.map(_.ensureCP[E, LL, EE](err)(cond)(liftCp, add)))
    }

    def ensureCP[E, LL, EE](err: => E)(cond: Result => Boolean)(
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

    def futMtA[E](implicit c: AndThen.Aux[Err, LiftCp.Aux, Align, E], ec: ExecutionContext): FutureEitherMT[E, Res] = {
      FutureEitherMT(Future.successful(e.left.map(err => c.b(c.a(err)))))
    }

    def align[E](implicit c: AndThen.Aux[Err, LiftCp.Aux, Align, E]): Either[E, Res] = e.left.map(err => c.b(c.a(err)))

    def partiallyHandle[L, R <: Coproduct, O, RR >: Res](f: Err => MatchSyntax.Case[L +: R, RR])(
      implicit _if: IF.Aux[R =:= CNil, L, L +: R, O]
    ): Either[O, RR] = e.left.map(err => f(err).suspend).joinLeft


    def handle[RR >: Res](f: Err => RR): RR = e.valueOr(f)

    def or[Err1, ErrCP <: Coproduct, Err1CP <: Coproduct, O](e1: Either[Err1, Res])(
      implicit le: LiftCp.Aux[Err, ErrCP], le1: LiftCp.Aux[Err1, Err1CP], m: Merge.Aux[ErrCP, Err1CP, O]
    ): Either[O, Res] = {
      //avoiding the runtime repacking
      def cast(e: Either[Any, Res]) = e.asInstanceOf[Either[O, Res]]
      e.fold(
        err => e1.fold(
          err1 => Left(m.prepend(le1(err1))),
          _ => cast(e1)
        ),
        _ => cast(e)
      )
    }

    def joinCP[Err2, Res2, LC1, LC2, MO](
      implicit ev: Res <:< Either[Err2, Res2], liftCp: LiftCp.Aux[Err, LC1], liftCp2: LiftCp.Aux[Err2, LC2], m: Merge.Aux[LC1, LC2, MO]
    ): Either[MO, Res2] = {
      e.fold(
        l => Left(m.append(liftCp(l))),
        r => ev(r).left.map(l => m.prepend(liftCp2(l)))
      )
    }

    def ensureCP[E, LL, EE](ll: => E)(cond: Res => Boolean)(implicit liftCp: LiftCp.Aux[Err, LL], add: Add.Aux[LL, E, EE]): Either[EE, Res] = {
      ensureCP[E, LL, EE]((_: Res) => ll)(cond)(liftCp, add)
    }

    def ensureCP[E, LL, EE](ll: Res => E)(cond: Res => Boolean)(implicit liftCp: LiftCp.Aux[Err, LL], add: Add.Aux[LL, E, EE]): Either[EE, Res] = {
      e match {
        case Left(_err) => Left(add.extend(liftCp(_err)))
        case x@Right(res) => if (cond(res)) x.asInstanceOf[Either[EE, Res]] else Left(add(ll(res)))
      }
    }


    def hasError[E](implicit extract: ExtractInvariant[Err, E]): Boolean = e.isLeft && extract(e.left.get).isRight

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

  implicit class TryOps[T](t: Try[T]) {

    def asEither: Either[Throwable, T] = t match {
      case Failure(e: Throwable) => if (NonFatal(e)) Left(e) else throw e
      case Success(res) => Right(res)
    }

    def wrapWith[Err](f: Throwable => Err): Either[Err, T] = asEither.left.map(f)

    def wrapWithErr(msg: String = ""): Either[GenErr, T] = t match {
      case Failure(e: Throwable) => if (NonFatal(e)) Left(GenErr(msg, Some(Right(e)))) else throw e
      case Success(res) => Right(res)
    }
  }

  //workaround for scalas strange bahaviour.Cannot throw exception from _case[T], compiler cannot find IF instance if Res type == Nothing
  def _throw[T <: Throwable](t: T): Null = throw t

  def retry[T](v: => T, n: Int)(p: T => Boolean): T = {
    if (n == 1) {
      v
    }
    else {
      val res = v
      if (p(res)) {
        retry(v, n - 1)(p)
      }
      else {
        res
      }
    }

  }

  def retryAsync[T](v: => Future[T], n: Int)(p: T => Boolean)(implicit ec: ExecutionContext): Future[T] = {
    if (n == 1) {
      v
    }
    else {
      val resFut: Future[T] = v
      resFut.flatMap { res =>
        if (p(res)) {
          retryAsync(v, n - 1)(p)
        }
        else {
          resFut
        }
      }
    }
  }
}
