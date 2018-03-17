package coproduct.ops

import coproduct.Coproduct._
import shapeless.Coproduct
import shapeless.{Inl, Inr}

trait Mapper [C, S, D, V] {
  type Out
  def apply(u: C, f: S => D): Out
}

object Mapper{
  type Aux[C, S, D, O, V] = Mapper[C, S, D, V] {type Out = O}
  implicit def cov[C <: Coproduct, S, D, O <: Coproduct](implicit e: MapperInvariant.Aux[C, S, D, O]): Aux[C, S, D, O, Invariant] = e
  implicit def inv[C <: Coproduct, S, D, O <: Coproduct](implicit e: MapperCovariant.Aux[C, S, D, O]): Aux[C, S, D, O, Covariant] = e
}

trait MapperInvariant[C, S, D] extends Mapper[C, S, D, Invariant] {
  type Out
  def apply(u: C, f: S => D): Out
}

object MapperInvariant {

  type Aux[U, S, D, O] = MapperInvariant[U, S, D] {type Out = O}

  implicit def right[L, R <: Coproduct, D]: MapperInvariant.Aux[L +: R, L, D, D +: R] = new MapperInvariant[L +: R, L, D] {
    type Out = D +: R
    def apply(u: L +: R, f: L => D): Out = u.eliminate(l => Inl(f(l)), r => Inr[D, R](r))
  }

  implicit def recurse[L, R <: Coproduct, RO <: Coproduct, S, D]
  (implicit map: MapperInvariant.Aux[R, S, D, RO]): MapperInvariant.Aux[L +: R, S, D, L +: RO] = new MapperInvariant[L +: R, S, D] {
    type Out = L +: RO
    override def apply(u: L +: R, f: (S) => D): Out = u.eliminate(Inl(_), r => Inr(map(r, f)))
  }
}

trait MapperCovariant [C, S, D] extends Mapper[C, S, D, Covariant] {
  type Out
  def apply(u: C, f: S => D): Out
}

object MapperCovariant {
  type Aux[U, S, D, O] = MapperCovariant[U, S, D] {type Out = O}

  implicit def instance[C <: Coproduct, S,D, O <: Coproduct](
    implicit extract: ExtractCovariant.Aux[C, S, O, MatchRequired]
  ): Aux[C, S, D, D +: O] = new MapperCovariant[C, S, D] {
    type Out = D +: O
    def apply(u: C,  f: S => D): Out = extract(u).fold(
      l => Inr(l),
      r => Inl(f(r))
    )
  }
}