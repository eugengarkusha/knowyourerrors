package errorhandling.coproduct.ops

import errorhandling.coproduct.Coproduct._
import errorhandling.misc.boolOps.IF
import shapeless.ops.coproduct.Remove
import shapeless.{<:!<, CNil, Coproduct, Inl, Inr}

trait VarianceType

trait Covariant extends VarianceType

trait Invariant extends VarianceType

//Type class that removes 'V from coproduct 'C' and returns V if only CNil ledt in coproduct or Erither[Rest, V]
//the return type is determined on type level
//used for syntax

trait Extract[C, V, Type <: VarianceType] {
  type Rest
  type Out
  def apply(a: C): Out
}

object Extract {

  type Aux[C, V, R <: Coproduct, O, T <: VarianceType] = Extract[C, V, T] {
    type Out = O
    type Rest = R
  }

  implicit def cov[C <: Coproduct, V, R <: Coproduct, IFO](
      implicit r: RemoveCovariant.Aux[C, V, R],
      _if: IF.Aux[R =:= CNil, DummyImplicit, V, Either[R, V], IFO]): Extract.Aux[C, V, R, IFO, Covariant] =
    new Extract[C, V, Covariant] {
      type Out = IFO
      type Rest = R
      def apply(a: C): IFO = _if(_ => r(a).ensuring(_.isRight).right.get, _ => r(a))
    }

  implicit def inv[C <: Coproduct, V, R <: Coproduct, IFO](
      implicit r: Remove.Aux[C, V, R],
      _if: IF.Aux[R =:= CNil, DummyImplicit, V, Either[R, V], IFO]
  ): Extract.Aux[C, V, R, IFO, Invariant] = new Extract[C, V, Invariant] {
    type Out = IFO
    type Rest = R
    def apply(a: C): IFO = _if(_ => r(a).ensuring(_.isLeft).left.get, _ => r(a).swap)
  }
}

trait RemoveCovariant[C, S] {
  type Rest <: Coproduct
  def apply(c: C): Either[Rest, S]
}

object RemoveCovariant {

  type Aux[C, S, O] = RemoveCovariant[C, S] {
    type Rest = O
  }

  implicit def noMatch[L, S, R <: Coproduct, O <: Coproduct](
      implicit extract: RemoveCovariant.Aux[R, S, O],
      ev: L <:!< S
  ): RemoveCovariant.Aux[L +: R, S, L +: O] = new RemoveCovariant[L +: R, S] {
    type Rest = L +: O
    def apply(c: L +: R): Either[Rest, S] = c.eliminate(l => Left(Inl(l)), r => extract(r).left.map(Inr(_)))
  }

  implicit def _match[S, L, R <: Coproduct, O <: Coproduct](
      implicit extract: RemoveCovariant.Aux[R, S, O],
      ev: L <:< S
  ): RemoveCovariant.Aux[L +: R, S, O] = new RemoveCovariant[L +: R, S] {
    type Rest = O
    def apply(c: L +: R): Either[Rest, S] = c.eliminate(l => Right(ev(l)), r => extract(r))
  }

  implicit def terminal[S, L, R <: Coproduct](implicit ev: L <:< S): RemoveCovariant.Aux[CNil, S, CNil] =
    new RemoveCovariant[CNil, S] {
      type Rest = CNil
      def apply(c: CNil): Either[Rest, S] = throw new AssertionError("got value of type CNil")
    }

}
