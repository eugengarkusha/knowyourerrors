package errorhandling.coproduct.ops

import errorhandling.coproduct.Coproduct._
import shapeless.{<:!<, Coproduct, CNil, Inl, Inr}

//first tries to inject an element (injectR method) and if element does not exist in coproduct it prepends the element

trait Add[C, V] {
  type Out
  def apply(a: V): Out
  def extend(u: C): Out
}

trait prepend {
  implicit def prepend[C <: Coproduct, V]: Add.Aux[C, V, V +: C] = new Add[C, V] {
    override type Out = V +: C
    override def apply(a: V): Out = Inl[V, C](a)
    override def extend(u: C): Out = Inr[V, C](u)
  }
}

object Add extends prepend {
  type Aux[C, V, O] = Add[C, V] { type Out = O }

  implicit def cnil: Add.Aux[CNil, CNil, CNil] = new Add[CNil, CNil] {
    override type Out = CNil
    override def apply(v: CNil): Out = v
    override def extend(u: CNil): Out = u
  }

  implicit def injectL[V, R <: Coproduct]: Add.Aux[V +: R, V, V +: R] = new Add[V +: R, V] {
    override type Out = V +: R
    override def apply(v: V): Out = Inl[V, R](v)
    override def extend(u: V +: R): Out = u
  }

  implicit def injectR[L, R <: Coproduct, V](implicit add: Add.Aux[R, V, R]): Add.Aux[L +: R, V, L +: R] =
    new Add[L +: R, V] {
      override type Out = L +: R
      override def apply(a: V): Out = Inr[L, R](add(a))
      override def extend(u: L +: R): Out = u match {
        case cl: Inl[L, R] => cl
        case Inr(r)        => Inr(add.extend(r))
      }
    }

  class AddSyntax[V](a: V) {
    //disallowing empty coproducts. Use Coproduct.empty to create empty one
    def to[C <: Coproduct](implicit i: Add[C, V], ev: V <:!< CNil): i.Out = i(a)
  }

  def apply[V](a: V): AddSyntax[V] = new AddSyntax(a)
}
