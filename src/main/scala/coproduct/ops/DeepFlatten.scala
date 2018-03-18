package coproduct.ops

import shapeless.{=:!=, Coproduct, Poly1}

//TODO: reuse shapeless flatmap with id polyfunc(multiple times) and then dedup in syntax (dont mixt dedup in(unless it hits compilation performance(save this code before rewriting))
// Ensures that Coproduct does not contain nested coproducts and duplicate elements
trait DeepFlatten[C<: Coproduct] {
  type Out<: Coproduct
  def apply(c: C): Out
}

object DeepFlatten{

  def apply[T <: Coproduct](implicit f: DeepFlatten[T]): DeepFlatten.Aux[T, f.Out] = f
  type Aux[C<: Coproduct, O<: Coproduct] = DeepFlatten[C]{type Out = O}

  object ident extends Poly1 {
    implicit def any[T](implicit lc: LiftCp[T]) = at[T](lc(_))
  }

  type Flatten[C<: Coproduct, O<: Coproduct] = shapeless.ops.coproduct.FlatMap.Aux[C,ident.type , O]

  implicit def flatten[C <: Coproduct, O <: Coproduct, DFO <: Coproduct](
    implicit m: Flatten[C, O],
    ev: C =:!= O,
    f1: DeepFlatten.Aux[O, DFO]
  ): DeepFlatten.Aux[C, DFO] = new DeepFlatten[C] {
    type Out = DFO
    def apply(c: C): Out =f1(m(c))
  }

  implicit def flatten1[C <: Coproduct](implicit  m: Flatten[C, C]): DeepFlatten.Aux[C, C] = new DeepFlatten[C] {
    type Out = C
    def apply(c: C): Out = c
  }
}

