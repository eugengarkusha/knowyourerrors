package errorhandling.syntax

import cats.Functor
import cats.data.EitherT
import errorhandling.coproduct.Coproduct.+:
import errorhandling.coproduct.ops.{Add, LiftCp}
import errorhandling.misc.boolOps.IF
import shapeless.{CNil, Coproduct}

trait EitherTSyntax { self: EitherSyntax =>

  implicit class EitherTOps[M[_], Err, Result](t: EitherT[M, Err, Result]) {

    def embed[E <: Coproduct](implicit c: Embed[Err, E], fnc: Functor[M]): EitherT[M, E, Result] = t.leftMap(c)

    def partiallyHandle[L, R <: Coproduct, O, RR >: Result](f: Err => Either[L +: R, RR])(
        implicit _if: IF.Aux[R =:= CNil, DummyImplicit, L, L +: R, O],
        fnc: Functor[M]
    ): EitherT[M, O, RR] = {
      EitherT(fnc.map(t.value)(_.partiallyHandle(f)))
    }

    def handle[RR >: Result](f: Err => RR)(implicit fnc: Functor[M]): M[RR] = t.valueOr(f)

    def ensureCP[E, LL <: Coproduct, EE](err: Result => E)(cond: Result => Boolean)(
        implicit liftCp: LiftCp.Aux[Err, LL],
        add: Add.Aux[LL, E, EE],
        fnc: Functor[M]
    ): EitherT[M, EE, Result] = {
      EitherT(fnc.map(t.value)(_.ensureCP[E, LL, EE](err)(cond)(liftCp, add)))
    }

    def ensureCP[E, LL <: Coproduct, EE](err: => E)(cond: Result => Boolean)(
        implicit liftCp: LiftCp.Aux[Err, LL],
        add: Add.Aux[LL, E, EE],
        fnc: Functor[M]
    ): EitherT[M, EE, Result] = {
      ensureCP[E, LL, EE]((_: Result) => err)(cond)(liftCp, add, fnc)
    }

  }

}
