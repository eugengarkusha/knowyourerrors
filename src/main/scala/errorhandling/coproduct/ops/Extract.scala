package errorhandling.coproduct.ops

import errorhandling.coproduct.Coproduct._
import shapeless.ops.coproduct.Remove
import shapeless.{<:!<, CNil, Coproduct, Inl, Inr}

trait VarianceType

trait Covariant extends VarianceType

trait Invariant extends VarianceType

// Version of shapeless.Remove allowing both Invariant (shapeless default) and covariant removal
// abstrating over Variance type is needed in MatchSyntax.ExtractSyntax(may also be needed elsewhere)
trait Extract[C, V, Type <: VarianceType] {
  type Rest <: Coproduct
  def apply(a: C): Either[Rest, V]
}

object Extract {

  type Aux[C, V, O <: Coproduct, T <: VarianceType] = Extract[C, V, T] { type Rest = O }

  implicit def cov[C <: Coproduct, V, O <: Coproduct](
      implicit e: ExtractCovariant.Aux[C, V, O]): Extract.Aux[C, V, O, Covariant] = e
  implicit def inv[C <: Coproduct, V, O <: Coproduct](
      implicit r: Remove.Aux[C, V, O]): Extract.Aux[C, V, O, Invariant] = new Extract[C, V, Invariant] {
    type Rest = O
    def apply(a: C): Either[Rest, V] = r(a).swap
  }
}

trait ExtractCovariant[C, S] extends Extract[C, S, Covariant] {
  type Rest <: Coproduct
  def apply(c: C): Either[Rest, S]
}

object ExtractCovariant {

  type Aux[C, S, O] = ExtractCovariant[C, S] {
    type Rest = O
  }

  implicit def noMatch[L, S, R <: Coproduct, O <: Coproduct](
      implicit extract: ExtractCovariant.Aux[R, S, O],
      ev: L <:!< S
  ): ExtractCovariant.Aux[L +: R, S, L +: O] = new ExtractCovariant[L +: R, S] {
    type Rest = L +: O
    def apply(c: L +: R): Either[Rest, S] = c.eliminate(l => Left(Inl(l)), r => extract(r).left.map(Inr(_)))
  }

  implicit def _match[S, L, R <: Coproduct, O <: Coproduct](
      implicit extract: ExtractCovariant.Aux[R, S, O],
      ev: L <:< S
  ): ExtractCovariant.Aux[L +: R, S, O] = new ExtractCovariant[L +: R, S] {
    type Rest = O
    def apply(c: L +: R): Either[Rest, S] = c.eliminate(l => Right(ev(l)), r => extract(r))
  }

  implicit def terminal[S, L, R <: Coproduct](implicit ev: L <:< S): ExtractCovariant.Aux[CNil, S, CNil] =
    new ExtractCovariant[CNil, S] {
      type Rest = CNil
      def apply(c: CNil): Either[Rest, S] = throw new AssertionError("got value of type CNil")
    }

}
