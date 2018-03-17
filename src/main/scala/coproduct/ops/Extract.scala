package coproduct.ops


import coproduct.Coproduct._
import shapeless.{<:!<, Coproduct, Inl, Inr, CNil}


trait VarianceType

trait Covariant extends VarianceType

trait Invariant extends VarianceType

//abstrating over Variance type is needed in MatchSyntax.ExtractSyntax(may also be needed elsewhere)
trait Extract[C, V, Type <: VarianceType] {
  type Out <: Coproduct
  def apply(a: C): Either[Out, V]
}

object Extract {

  type Aux[C, V, O <: Coproduct, T <: VarianceType] = Extract[C, V, T] {type Out = O}

  implicit def cov[C <: Coproduct, V, O <: Coproduct](implicit e: ExtractCovariant.Aux[C, V, O, MatchRequired]): Extract.Aux[C, V, O, Covariant] = e
  implicit def inv[C <: Coproduct, V, O <: Coproduct](implicit e: ExtractInvariant.Aux[C, V, O]): Extract.Aux[C, V, O, Invariant] = e
}

//V is required to be present in coproduct
trait ExtractInvariant[C, V] extends Extract[C, V, Invariant] {
  type Out <: Coproduct
  def apply(a: C): Either[Out, V]
}

object ExtractInvariant {

  type Aux[C, V, R] = ExtractInvariant[C, V] {type Out = R}

  implicit def found[V, R <: Coproduct]: ExtractInvariant.Aux[V +: R, V, R] = new ExtractInvariant[V +: R, V] {
    type Out = R
    def apply(a: V +: R): Either[Out, V] = a.eliminate(Right(_), Left(_))
  }

  implicit def notFound[V, L, R <: Coproduct, RO <: Coproduct]
  (implicit extract: ExtractInvariant.Aux[R, V, RO]): ExtractInvariant.Aux[L +: R, V, L +: RO] = new ExtractInvariant[L +: R, V] {
    type Out = L +: RO
    def apply(a: L +: R): Either[Out, V] = a.eliminate(l => Left(Inl[L, RO](l)), r => extract(r).left.map(Inr[L, RO](_)))
  }

  class ExtractSyntax[V] {
    def from[C <: Coproduct](e: C)(implicit extract: ExtractInvariant[C, V]): Either[extract.Out, V] = extract.apply(e)
  }

  def apply[A]: ExtractSyntax[A] = new ExtractSyntax[A]

  def _type[V, C <: Coproduct](implicit extract: ExtractInvariant[C, V]): ExtractInvariant.Aux[C, V, extract.Out] = extract

}

//think of removing this control
trait MatchControl
trait MatchRequired extends MatchControl
trait NoMatchRequired extends MatchControl

trait ExtractCovariant[C, S, +FF <: MatchControl] extends Extract[C, S, Covariant] {
  type Out <: Coproduct
  def apply(c: C): Either[Out, S]
}

object ExtractCovariant {

  type OptionalMatch = MatchControl

  type Aux[C, S, O, Ctrl <: MatchControl] = ExtractCovariant[C, S, Ctrl] {
    type Out = O
  }

  implicit def noMatch[L, S, R <: Coproduct, Ctrl <: MatchControl, O <: Coproduct](
    implicit extract: ExtractCovariant.Aux[R, S, O, Ctrl],
    ev: L <:!< S
  ): ExtractCovariant.Aux[L +: R, S, L +: O, Ctrl] = new ExtractCovariant[L +: R, S, Ctrl] {
    type Out = L +: O
    def apply(c: L +: R): Either[Out, S] = c.eliminate(l => Left(Inl(l)), r => extract(r).left.map(Inr(_)))
  }

  implicit def _match[S, L, Ctrl <: MatchControl, R <: Coproduct, O <: Coproduct](
    implicit extract: ExtractCovariant.Aux[R, S, O, Ctrl],
    ev: L <:< S
  ): ExtractCovariant.Aux[L +: R, S, O, MatchRequired] = new ExtractCovariant[L +: R, S, MatchRequired] {
    type Out = O
    def apply(c: L +: R): Either[Out, S] = c.eliminate(l => Right(ev(l)), r => extract(r))
  }

  implicit def cnil[S]: ExtractCovariant.Aux[CNil, S, CNil, NoMatchRequired] = new ExtractCovariant[CNil, S, NoMatchRequired] {
    type Out = CNil
    def apply(c: CNil): Either[Out, S] = Left(c)
  }
}



