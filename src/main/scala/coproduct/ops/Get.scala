package coproduct.ops


import coproduct.Coproduct._
import shapeless.Coproduct
import shapeless.{Lub, CNil}

trait Get[C]{
  type Out
  def apply(u: C): Option[Out]
}
object Get{
  type Aux[C, O] = Get[C]{type Out = O}
  implicit def recurse[L, R <: Coproduct, RO , O](implicit get: Get.Aux[R, RO], lub: Lub[L, RO, O] ): Get.Aux[L +: R, O] = new Get[L +: R]{
    type Out = O
    def apply(u: L +: R): Option[O] = u.eliminate(l=> Some(lub.left(l)), get(_).map(lub.right(_)))
  }

  implicit def cnil[L]: Get.Aux[L +: CNil, L] = new Get[L +: CNil]{
    type Out = L
    def apply(u: L +: CNil): Option[L] = u.eliminate(Some(_), _=> None)
  }
}
