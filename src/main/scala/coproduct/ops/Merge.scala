package coproduct.ops

import coproduct.Coproduct._
import shapeless.Coproduct
import shapeless.{CNil, Inr}
//merges values into single coproduct(no deduplication/flattening)

trait Merge[A, B] {
  type Out
  def append(c: A): Out
  def prepend(c: B): Out
}

trait CNilR {
  self: Merge.type =>
  //distinguishing beetween cnilR and cnilL for performance
  implicit def cnilR[A <: Coproduct](implicit add: Add.Aux[A, CNil, A]): Aux[A, CNil, A] = new Merge[A, CNil] {
    type Out = A
    def append(a: A): A = a
    def prepend(a: CNil): A = add(a)
  }

}

object Merge extends CNilR /*with plain */{

  type Aux[A, B, O] = Merge[A, B] {type Out = O}

  implicit def cnilL[A <: Coproduct](implicit add: Add.Aux[A, CNil, A]): Aux[CNil, A, A] = new Merge[CNil, A] {
    type Out = A
    def append(a: CNil): A = add(a)
    def prepend(a: A): A = a
  }

  implicit def recurse[L, R <: Coproduct, L1, R1 <: Coproduct, MO <: Coproduct]
  (implicit p: Aux[R, L1 +: R1, MO]): Aux[L +: R, L1 +: R1, L +: MO] = new Merge[L +: R, L1 +: R1] {
    type Out = L +: MO
    def append(a: L +: R): Out = a.map(r => p.append(r))
    def prepend(b: L1 +: R1): L +: MO = Inr(p.prepend(b))
  }
}

