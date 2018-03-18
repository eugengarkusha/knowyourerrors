package coproduct.ops

import coproduct.Coproduct._
import shapeless.{Coproduct, CNil}

//Deduplicates coproduct types. For internal use only(it should not be reasonable co construct coproduct with duplicates)
//See 'Flatten' for combining coproducts like C1 +: C2

//adds all left elements of a coproduct to right side recursively (Add operation will remove the type if it already exists)
//no runtime operations are performed if coproduct has distinct elements

trait Dedup[C] {
  type Out
  def apply(u: C): Out
}

trait DedupX[C] extends Dedup[C]

trait lo {
  implicit def dedup[L, R <: Coproduct, RO <: Coproduct, O](implicit dedup: Dedup.XAux[R, RO],
                                                            add: Add.Aux[RO, L, O]): Dedup.XAux[L +: R, O] =
    new DedupX[L +: R] {
      type Out = O
      def apply(u: L +: R): Out = u.eliminate(add(_), r => add.extend(dedup(r)))
    }
}
object Dedup extends lo {

  type Aux[C, O] = Dedup[C] { type Out = O }
  type XAux[C, O] = DedupX[C] { type Out = O }

  def apply[C <: Coproduct](implicit d: Dedup[C]): Dedup.Aux[C, d.Out] = d

  implicit def cnil: XAux[CNil, CNil] = new DedupX[CNil] {
    type Out = CNil
    def apply(u: CNil): CNil = u
  }

  //eliminates the runtime complexity for distinct coproducts
  implicit def noDuplicates[C <: Coproduct, O <: Coproduct](implicit notUsed: Dedup.XAux[C, O],
                                                            ev: C =:= O): Dedup.Aux[C, O] = new Dedup[C] {
    type Out = O
    def apply(c: C): O = ev(c)
  }

}
