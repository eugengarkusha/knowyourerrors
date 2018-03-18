package errorhandling

import cats.Functor
import cats.data.EitherT
import cats.syntax.either._
import cats.syntax.functor._
import errorhandling.coproduct.Coproduct._
import errorhandling.coproduct.ops.{LiftCp, MatchSyntax, _}
import errorhandling.misc.boolOps._
import shapeless.ops.coproduct.{Basis, Prepend}
import shapeless.{=:!=, CNil, Coproduct}

object syntax {

  trait Embed[Err, Super <: Coproduct] extends (Err => Super)
  object Embed {
    implicit def x[Err, Lerr <: Coproduct, Super <: Coproduct](implicit lcp: LiftCp.Aux[Err, Lerr],
                                                               e: Basis[Super, Lerr]): Embed[Err, Super] =
      v => e.inverse(Right(lcp(v)))
  }

  implicit class EitherTSyntax[M[_]: Functor, Err, Result](t: EitherT[M, Err, Result]) {

    def embed[E <: Coproduct](implicit c: Embed[Err, E]): EitherT[M, E, Result] = t.leftMap(c)

    def partiallyHandle[L, R <: Coproduct, O, RR >: Result](f: Err => MatchSyntax.Case[L +: R, RR])(
        implicit _if: IF.Aux[R =:= CNil, L, L +: R, O]
    ): EitherT[M, O, RR] = {
      EitherT(t.value.map(_.partiallyHandle(f)))
    }
    def handle[RR >: Result](f: Err => RR): M[RR] = t.valueOr(f)

    def ensureCP[E, LL <: Coproduct, EE](err: Result => E)(cond: Result => Boolean)(
        implicit liftCp: LiftCp.Aux[Err, LL],
        add: Add.Aux[LL, E, EE]
    ): EitherT[M, EE, Result] = {
      EitherT(t.value.map(_.ensureCP[E, LL, EE](err)(cond)(liftCp, add)))
    }

    def ensureCP[E, LL <: Coproduct, EE](err: => E)(cond: Result => Boolean)(
        implicit liftCp: LiftCp.Aux[Err, LL],
        add: Add.Aux[LL, E, EE]
    ): EitherT[M, EE, Result] = {
      ensureCP[E, LL, EE]((_: Result) => err)(cond)(liftCp, add)
    }

  }

  // Err must be inferred
  implicit class EitherSyntax[Err, Res](e: Either[Err, Res])(implicit ev: Err =:!= Nothing) {

    def embed[E <: Coproduct](implicit c: Embed[Err, E]): Either[E, Res] = e.left.map(c)

    def partiallyHandle[L, R <: Coproduct, O, RR >: Res](f: Err => MatchSyntax.Case[L +: R, RR])(
        implicit _if: IF.Aux[R =:= CNil, L, L +: R, O]
    ): Either[O, RR] = e.left.map(err => f(err).suspend).joinLeft

    def handle[RR >: Res](f: Err => RR): RR = e.valueOr(f)

    def or[Err1, ErrCP <: Coproduct, Err1CP <: Coproduct, O <: Coproduct](e1: Either[Err1, Res])(
        implicit le: LiftCp.Aux[Err, ErrCP],
        le1: LiftCp.Aux[Err1, Err1CP],
        m: Prepend.Aux[ErrCP, Err1CP, O]
    ): Either[O, Res] = {
      //avoiding the runtime repacking
      def cast(e: Either[Any, Res]) = e.asInstanceOf[Either[O, Res]]
      e.fold(
        err =>
          e1.fold(
            err1 => Left(m(Right(le1(err1)))),
            _ => cast(e1)
        ),
        _ => cast(e)
      )
    }

    def joinCP[Err2, Res2, LC1 <: Coproduct, LC2 <: Coproduct, MO <: Coproduct](
        implicit ev: Res <:< Either[Err2, Res2],
        liftCp: LiftCp.Aux[Err, LC1],
        liftCp2: LiftCp.Aux[Err2, LC2],
        m: Prepend.Aux[LC1, LC2, MO]
    ): Either[MO, Res2] = {
      e.fold(
        l => Left(m(Left(liftCp(l)))),
        r => ev(r).left.map(l => m(Right(liftCp2(l))))
      )
    }

    def ensureCP[E, LL <: Coproduct, EE](ll: => E)(cond: Res => Boolean)(implicit liftCp: LiftCp.Aux[Err, LL],
                                                                         add: Add.Aux[LL, E, EE]): Either[EE, Res] = {
      ensureCP[E, LL, EE]((_: Res) => ll)(cond)(liftCp, add)
    }

    def ensureCP[E, LL <: Coproduct, EE](ll: Res => E)(
        cond: Res => Boolean)(implicit liftCp: LiftCp.Aux[Err, LL], add: Add.Aux[LL, E, EE]): Either[EE, Res] = {
      e match {
        case Left(_err)     => Left(add.extend(liftCp(_err)))
        case x @ Right(res) => if (cond(res)) x.asInstanceOf[Either[EE, Res]] else Left(add(ll(res)))
      }
    }

    def hasError[E](implicit extract: Extract[Err, E, Invariant]): Boolean = e.isLeft && extract(e.left.get).isRight

  }
}
