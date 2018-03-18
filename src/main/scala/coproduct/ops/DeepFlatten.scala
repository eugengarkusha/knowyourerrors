package coproduct.ops

import coproduct.Coproduct._
import shapeless.ops.coproduct.Prepend
import shapeless.{CNil, Coproduct}

// Ensures that Coproduct does not contain nested coproducts and duplicate elements
trait DeepFlatten[C<: Coproduct] {
  type Out<: Coproduct
  def apply(c: C): Out
}

trait loFlatten{
  implicit def recurse[L, R <: Coproduct, FR <: Coproduct, AO <: Coproduct]
  (implicit f: DeepFlatten.Aux[R, FR], a: Add.Aux[FR, L, AO]): DeepFlatten.Aux[L +: R, AO] = new DeepFlatten [L +: R] {
    type Out = AO
    def apply(u: L +: R): Out = u.eliminate(l=> a(l), r => a.extend((f(r))))
  }
}
object DeepFlatten extends loFlatten{

  type Aux[C<: Coproduct, O<: Coproduct] = DeepFlatten[C]{type Out = O}

  def apply[T <: Coproduct](implicit f: DeepFlatten[T]): DeepFlatten.Aux[T, f.Out] = f

  implicit def flatten[L <: Coproduct, R <: Coproduct, PO <: Coproduct, O <: Coproduct ]
  (implicit  m: Prepend.Aux[L, R, PO], f1: DeepFlatten.Aux[PO, O]): DeepFlatten.Aux[L +: R, O] = new DeepFlatten[L +: R] {
    type Out = O
    def apply(u: L +: R): Out = u.eliminate(l => f1(m(Left(l))), r => f1(m(Right(r))))
  }

  implicit def cnil: DeepFlatten.Aux[CNil, CNil] = new DeepFlatten[CNil]{
    override type Out = CNil
    override def apply(c: CNil): Out = c
  }
}
