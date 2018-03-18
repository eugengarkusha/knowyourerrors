package errorhandling.coproduct.ops

import errorhandling.coproduct.Coproduct._
import shapeless.{<:!<, =:!=, CNil, Coproduct, Inl, Inr}

//Maps monofunction over coproduct(covariant and invariant modes are supported). Using shapeless.PodyDefns.-> requires separate object declaration(cannot be used inline)
trait MonoMap[C, S, D, V] {
  type Out
  def apply(u: C, f: S => D): Out
}

object MonoMap {
  type Aux[C, S, D, O, V] = MonoMap[C, S, D, V] { type Out = O }
  implicit def cov[C <: Coproduct, S, D, O <: Coproduct](
      implicit e: MonoMapInvariant.Aux[C, S, D, O]): Aux[C, S, D, O, Invariant] = e
  implicit def inv[C <: Coproduct, S, D, O <: Coproduct](
      implicit e: MonoMapCovariant.Aux[C, S, D, O]): Aux[C, S, D, O, Covariant] = e
}

trait MonoMapInvariant[C, S, D] extends MonoMap[C, S, D, Invariant] {
  type Out
  def apply(u: C, f: S => D): Out
}

object MonoMapInvariant {

  type Aux[U, S, D, O] = MonoMapInvariant[U, S, D] { type Out = O }

  implicit def cnil[S, D]: MonoMapInvariant.Aux[CNil, S, D, CNil] = new MonoMapInvariant[CNil, S, D] {
    type Out = CNil
    def apply(u: CNil, f: S => D): Out = u
  }

  implicit def right[S, T <: Coproduct, D, TM <: Coproduct](
      implicit m: MonoMapInvariant.Aux[T, S, D, TM]): MonoMapInvariant.Aux[S +: T, S, D, D +: TM] =
    new MonoMapInvariant[S +: T, S, D] {
      type Out = D +: TM
      def apply(u: S +: T, f: S => D): Out = u.eliminate(l => Inl(f(l)), r => Inr(m(r, f)))
    }

  implicit def recurse[L, S, R <: Coproduct, RO <: Coproduct, D](
      implicit ev: L =:!= S,
      map: MonoMapInvariant.Aux[R, S, D, RO]): MonoMapInvariant.Aux[L +: R, S, D, L +: RO] =
    new MonoMapInvariant[L +: R, S, D] {
      type Out = L +: RO
      override def apply(u: L +: R, f: (S) => D): Out = u.eliminate(Inl(_), r => Inr(map(r, f)))
    }
}

trait MonoMapCovariant[C, S, D] extends MonoMap[C, S, D, Covariant] {
  type Out
  def apply(u: C, f: S => D): Out
}

object MonoMapCovariant {

  type Aux[U, S, D, O] = MonoMapCovariant[U, S, D] { type Out = O }

  implicit def cnil[S, D]: MonoMapCovariant.Aux[CNil, S, D, CNil] = new MonoMapCovariant[CNil, S, D] {
    type Out = CNil
    def apply(u: CNil, f: S => D): Out = u
  }

  implicit def right[S, SS <: S, T <: Coproduct, D, TM <: Coproduct](
      implicit m: MonoMapCovariant.Aux[T, S, D, TM]): MonoMapCovariant.Aux[SS +: T, S, D, D +: TM] =
    new MonoMapCovariant[SS +: T, S, D] {
      type Out = D +: TM
      def apply(u: SS +: T, f: S => D): Out = u.eliminate(l => Inl(f(l)), r => Inr(m(r, f)))
    }

  implicit def recurse[L, S, R <: Coproduct, RO <: Coproduct, D](
      implicit ev: L <:!< S,
      map: MonoMapCovariant.Aux[R, S, D, RO]): MonoMapCovariant.Aux[L +: R, S, D, L +: RO] =
    new MonoMapCovariant[L +: R, S, D] {
      type Out = L +: RO
      override def apply(u: L +: R, f: (S) => D): Out = u.eliminate(Inl(_), r => Inr(map(r, f)))
    }
}
