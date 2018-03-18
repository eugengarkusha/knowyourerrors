package coproduct

import coproduct.ops.Add
import shapeless.{<:!<, Coproduct, CNil, Inl, Inr}

object Coproduct {
  type +:[L, R <: Coproduct] = shapeless.:+:[L, R]
  type :+:[L, R] = L +: R +: CNil
  def single[V](t: V)(implicit ev: V <:!< CNil): Left[V, CNil] = Left[V, CNil](t)

  implicit class CPOps[L, R <: Coproduct](val c: +:[L, R]) extends AnyVal {
    def map[V <: Coproduct](f: R => V): +:[L, V] = c match {
      case cl: Inl[L, R] => cl.asInstanceOf[+:[L, V]]
      case Inr(r)        => Inr(f(r))
    }
    def left: Option[L] = c.eliminate(Some(_), _ => None)
    def right: Option[R] = c.eliminate(_ => None, Some(_))

  }
}
