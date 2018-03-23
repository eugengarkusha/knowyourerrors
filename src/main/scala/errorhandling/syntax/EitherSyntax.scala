package errorhandling.syntax

import errorhandling.coproduct.Coproduct.+:
import errorhandling.coproduct.ops.{Add, Covariant, Invariant, LiftCp}
import errorhandling.misc.boolOps.IF
import shapeless.{=:!=, CNil, Coproduct}
import shapeless.ops.coproduct.{Basis, Prepend, Remove}
import cats.syntax.either._

trait EitherSyntax {

  implicit class EitherOps[Err, Res](e: Either[Err, Res])(implicit ev: Err =:!= Nothing) {

    def embed[E <: Coproduct](implicit c: Embed[Err, E]): Either[E, Res] = e.left.map(c)

    def partiallyHandle[L, R <: Coproduct, O, RR >: Res](f: Err => Either[L +: R, RR])(
        implicit _if: IF.Aux[R =:= CNil, DummyImplicit, L, L +: R, O]
    ): Either[O, RR] = e.fold(
      e => f(e).left.map(er => _if(_ => er.head.ensuring(_.isDefined).get, _ => er)),
      r => Right(r: RR)
    )

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

    def recoverFrom[V] = new RecoverSyntax[V, Err, Res, Invariant](e)
    def recoverFromAll[V] = new RecoverSyntax[V, Err, Res, Covariant](e)

    def hasError[E](implicit he: HasErr[Err, E]): Boolean = e.fold(he(_), _ => false)

  }
}
