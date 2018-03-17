package coproduct.ops


import coproduct.Coproduct._
import shapeless.{Coproduct,Inl,<:!<, CNil}
//Lifts T to Coproduct (if T is coproduct than just return T else return T :+ Cnil)
trait LiftCp[T] {
  type Out <: Coproduct
  def apply(t: T): Out
}
object LiftCp{

  private val _plain = new LiftCp[Any] {
    type Out = Inl[Any, Nothing]
    def apply(t: Any): Out = Inl[Any, Nothing](t)
  }
  private val _coproduct = new LiftCp[Any] {
    type Out = Coproduct
    def apply(t: Any): Out = t.asInstanceOf[Coproduct]
  }

  type Aux[T, O <: Coproduct] = LiftCp[T]{type Out = O}

  implicit def plain[T](implicit ev: T <:!< Coproduct): Aux[T, +:[T, CNil]] = _plain.asInstanceOf[LiftCp.Aux[T, T +: CNil]]
  implicit def cp[T <: Coproduct]: Aux[T, T] = _coproduct.asInstanceOf[LiftCp.Aux[T, T]]
}