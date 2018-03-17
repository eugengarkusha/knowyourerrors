package coproduct.ops

import coproduct.Coproduct._
import shapeless.{Coproduct, CNil}

trait Flatten[C] {
  type Out
  def apply(c: C): Out
}

trait lof{
  implicit def recurse[L, R <: Coproduct, FR <: Coproduct, AO <: Coproduct]
  (implicit  f: Flatten.Aux[R, FR], a: Add.Aux[FR, L, AO]): Flatten.Aux[L +: R, AO] = new Flatten [L +: R] {
    type Out = AO
    def apply(u: L +: R): Out = u.eliminate(l=> a(l), r => a.extend((f(r))))
  }
}
object Flatten extends lof{

  type Aux[C, O] = Flatten[C]{type Out = O}

  def apply[T <: Coproduct](implicit f: Flatten[T]): Flatten.Aux[T, f.Out] = f

  implicit def flatten[L <: Coproduct, R <: Coproduct, MO <: Coproduct , FO <: Coproduct, O <: Coproduct ]
  (implicit  m: Merge.Aux[L, R, MO], f1: Flatten.Aux[MO, O]): Flatten.Aux[L +: R, O] = new Flatten[L +: R] {
    type Out = O
    def apply(u: L +: R): Out = u.eliminate(l => f1(m.append(l)), r => f1(m.prepend(r)))
  }

  implicit def cnil: Flatten.Aux[CNil, CNil] = new Flatten[CNil]{
    override type Out = CNil
    override def apply(c: CNil): Out = c
  }
}
