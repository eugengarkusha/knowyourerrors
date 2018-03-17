package coproduct.ops

import coproduct.Coproduct._
import shapeless.{Coproduct, CNil}
import shapeless.ops.coproduct.Remove
//remove members of B from A

trait Diff [A, B]{
  type Out <: Coproduct
  def apply(a: A): Option[Out]
}

trait difflo {
  implicit def missRecurse[A <: Coproduct, BL, BR <: Coproduct, DO <: Coproduct](implicit d: Diff.Aux[A, BR, DO]): Diff.Aux[A, BL +: BR, DO]  = {
    new Diff[A, BL +: BR] {
      override type Out = DO
      override def apply(a: A): Option[DO] =  d(a)
    }
  }
}
object Diff extends difflo{
  type Aux[A, B, O] = Diff[A, B]{type Out = O}

  implicit def bCNil[A <: Coproduct] : Diff.Aux[A, CNil, A] = new Diff[A, CNil]{
    override type Out = A
    override def apply(a: A): Option[A] = Some(a)
  }

  implicit def matchRecurse[A <: Coproduct, BL, BR <: Coproduct, EO <: Coproduct, DO <: Coproduct]
  (implicit e: Remove.Aux[A, BL, EO] , diff: Diff.Aux[EO, BR, DO]):Diff.Aux[A, BL +: BR, DO]  = {
    new Diff[A, BL +: BR] {
      override type Out = DO
      override def apply(a: A): Option[DO] = e(a).fold(_ => None, diff(_))
    }
  }

}
