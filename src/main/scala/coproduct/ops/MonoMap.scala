package coproduct.ops

import coproduct.Coproduct._
import shapeless.Coproduct
import shapeless.{Inl, Inr}


// Applies a function to:
// -- covariant mode:  of subtype of T
// -- invarinat mode: first position of exact type T

//TODO: Make it Map all values of type T!!!!
//TODO2: Make mapCover respect order

// Would not need that if it would be possible to create shapeless polyfunctions (from monofunctions) as literals (i.e: without separate declaration)

//mapping monofunction over coproduct (and removing duplicates)
trait MonoMap [C, S, D, V] {
  type Out
  def apply(u: C, f: S => D): Out
}

object MonoMap{
  type Aux[C, S, D, O, V] = MonoMap[C, S, D, V] {type Out = O}
  implicit def cov[C <: Coproduct, S, D, O <: Coproduct](implicit e: MonoMapInvariant.Aux[C, S, D, O]): Aux[C, S, D, O, Invariant] = e
  implicit def inv[C <: Coproduct, S, D, O <: Coproduct](implicit e: MonoMapCovariant.Aux[C, S, D, O]): Aux[C, S, D, O, Covariant] = e
}

trait MonoMapInvariant[C, S, D] extends MonoMap[C, S, D, Invariant] {
  type Out
  def apply(u: C, f: S => D): Out
}

object MonoMapInvariant {

  type Aux[U, S, D, O] = MonoMapInvariant[U, S, D] {type Out = O}

  implicit def right[L, R <: Coproduct, D]: MonoMapInvariant.Aux[L +: R, L, D, D +: R] = new MonoMapInvariant[L +: R, L, D] {
    type Out = D +: R
    def apply(u: L +: R, f: L => D): Out = u.eliminate(l => Inl(f(l)), r => Inr[D, R](r))
  }

  implicit def recurse[L, R <: Coproduct, RO <: Coproduct, S, D]
  (implicit map: MonoMapInvariant.Aux[R, S, D, RO]): MonoMapInvariant.Aux[L +: R, S, D, L +: RO] = new MonoMapInvariant[L +: R, S, D] {
    type Out = L +: RO
    override def apply(u: L +: R, f: (S) => D): Out = u.eliminate(Inl(_), r => Inr(map(r, f)))
  }
}

trait MonoMapCovariant [C, S, D] extends MonoMap[C, S, D, Covariant] {
  type Out
  def apply(u: C, f: S => D): Out
}

object MonoMapCovariant {
  type Aux[U, S, D, O] = MonoMapCovariant[U, S, D] {type Out = O}

  implicit def instance[C <: Coproduct, S,D, O <: Coproduct](
    implicit extract: ExtractCovariant.Aux[C, S, O]
  ): Aux[C, S, D, D +: O] = new MonoMapCovariant[C, S, D] {
    type Out = D +: O
    def apply(u: C,  f: S => D): Out = extract(u).fold(
      l => Inr(l),
      r => Inl(f(r))
    )
  }
}