package coproduct.ops

import coproduct.Coproduct._
import shapeless.Coproduct
import shapeless.{Inl, Inr}


// Applies a function to:
// -- covariant mode:  of subtype of T
// -- invarinat mode: first position of exact type T

//TODO: Make it Map all values of type T!!!!, renme to monomap or smth
trait MapOne [C, S, D, V] {
  type Out
  def apply(u: C, f: S => D): Out
}

object MapOne{
  type Aux[C, S, D, O, V] = MapOne[C, S, D, V] {type Out = O}
  implicit def cov[C <: Coproduct, S, D, O <: Coproduct](implicit e: MapOneInvariant.Aux[C, S, D, O]): Aux[C, S, D, O, Invariant] = e
  implicit def inv[C <: Coproduct, S, D, O <: Coproduct](implicit e: MapOneCovariant.Aux[C, S, D, O]): Aux[C, S, D, O, Covariant] = e
}

trait MapOneInvariant[C, S, D] extends MapOne[C, S, D, Invariant] {
  type Out
  def apply(u: C, f: S => D): Out
}

object MapOneInvariant {

  type Aux[U, S, D, O] = MapOneInvariant[U, S, D] {type Out = O}

  implicit def right[L, R <: Coproduct, D]: MapOneInvariant.Aux[L +: R, L, D, D +: R] = new MapOneInvariant[L +: R, L, D] {
    type Out = D +: R
    def apply(u: L +: R, f: L => D): Out = u.eliminate(l => Inl(f(l)), r => Inr[D, R](r))
  }

  implicit def recurse[L, R <: Coproduct, RO <: Coproduct, S, D]
  (implicit map: MapOneInvariant.Aux[R, S, D, RO]): MapOneInvariant.Aux[L +: R, S, D, L +: RO] = new MapOneInvariant[L +: R, S, D] {
    type Out = L +: RO
    override def apply(u: L +: R, f: (S) => D): Out = u.eliminate(Inl(_), r => Inr(map(r, f)))
  }
}

trait MapOneCovariant [C, S, D] extends MapOne[C, S, D, Covariant] {
  type Out
  def apply(u: C, f: S => D): Out
}

object MapOneCovariant {
  type Aux[U, S, D, O] = MapOneCovariant[U, S, D] {type Out = O}

  implicit def instance[C <: Coproduct, S,D, O <: Coproduct](
    implicit extract: ExtractCovariant.Aux[C, S, O]
  ): Aux[C, S, D, D +: O] = new MapOneCovariant[C, S, D] {
    type Out = D +: O
    def apply(u: C,  f: S => D): Out = extract(u).fold(
      l => Inr(l),
      r => Inl(f(r))
    )
  }
}