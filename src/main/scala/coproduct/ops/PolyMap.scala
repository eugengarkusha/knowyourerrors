package coproduct.ops


import shapeless.Coproduct
import shapeless.{Inl, CNil}
import coproduct.Coproduct._
import shapeless.{HList, HNil}
import shapeless.::
import shapeless.ops.hlist.Reverse
import PolyMap.Entry
import misc.boolOps._

final class PolyFuncBuilder[H <: HList] private[ops](h: H) {

  final class MapSyntax[S, V <: VarianceType] {
    def apply[D](f: S => D): PolyFuncBuilder[Entry[S, D, V, MonoMap.Aux] :: H] = {
      val e = new Entry[S, D, V, MonoMap.Aux] {
        def apply[C, O](m: MonoMap.Aux[C, S, D, O, V], c: C): O = m(c, f)
      }
      new PolyFuncBuilder(e :: h)
    }
  }

  final class FlatMapSyntax[S, V <: VarianceType] {
    def apply[D](f: S => D): PolyFuncBuilder[Entry[S, D, V, DeepFlatMapper.Aux] :: H] = {
      val e = new Entry[S, D, V, DeepFlatMapper.Aux]{
        def apply[C, O](m: DeepFlatMapper.Aux[C, S, D, O, V], c: C): O = m(c, f)
      }
      new PolyFuncBuilder(e :: h)
    }
  }

  def mapI[S]: MapSyntax[S, Invariant] = new MapSyntax[S, Invariant]
  def mapC[S]: MapSyntax[S, Covariant] = new MapSyntax[S, Covariant]
  def flatMapC[S]: FlatMapSyntax[S, Covariant] = new FlatMapSyntax[S, Covariant]
  def flatMapI[S]: FlatMapSyntax[S, Invariant] = new FlatMapSyntax[S, Invariant]


  def done[O <: HList](implicit r: Reverse.Aux[H, O]): PolyFunc[O] = new PolyFunc(r(h))
}

final class PolyFunc[H <: HList] private[ops](val h: H) extends AnyVal {
  def apply[C, O](c: C)(implicit m: UniformPolyMap.Aux[C, H, O]): O = m(c, h)
}

trait PolyMap[C, H] {
  type Out
  def apply(c: C, h: H): Out
}

trait plo {
  implicit def recurseNoMatch[C, E, T <: HList, O](
    implicit p: PolyMap.Aux[C, T, O]
  ): PolyMap.Aux[C, E :: T, O] = {
    new PolyMap[C, E :: T] {
      override type Out = O
      override def apply(c: C, h: E :: T): Out = {
        p(c, h.tail)
      }
    }
  }
}

object PolyMap extends plo {
  type Aux[C, H, O] = PolyMap[C, H] {type Out = O}

  trait Entry[S, D, V, M[_, _, _, _, _]]{
    def apply[C, O](m: M[C, S, D, O, V], c: C): O
  }

  val builder: PolyFuncBuilder[HNil] = new PolyFuncBuilder[HNil](HNil)

  implicit def recurseMatch[C, S, D, O, V, M[_, _, _, _, _], T <: HList, O1](
    implicit m: M[C, S, D, O, V], p: PolyMap.Aux[O, T, O1]
  ): PolyMap.Aux[C, Entry[S, D, V, M] :: T, O1] = {
    new PolyMap[C, Entry[S, D, V, M] :: T] {
      override type Out = O1
      override def apply(c: C, h: Entry[S, D, V, M] :: T): Out = {
        p(h.head.apply(m, c), h.tail)
      }
    }
  }

  implicit def hn[C]: PolyMap.Aux[C, HNil, C] = new PolyMap[C, HNil] {
    override type Out = C
    override def apply(c: C, h: HNil): Out = c
  }

}

//PolyMap applicator that can be applied to both coproduct and plain types
trait UniformPolyMap[T, F <: HList]{
  type Out
  def apply (t: T, f: F): Out
}
object UniformPolyMap {

  type Aux[T, F <: HList, O] = UniformPolyMap[T, F]{type Out = O}

  implicit def instance[T, LO1 <: Coproduct, F <: HList, V1, Tl1 <: Coproduct, MO1<:Coproduct, IFO1](
    implicit l: LiftCp.Aux[T, LO1],
    m: PolyMap.Aux[LO1, F, MO1],
    ev: MO1 <:< +:[V1, Tl1],
    _if: IF.Aux[Tl1 <:< CNil, V1, MO1, IFO1]
  ): UniformPolyMap.Aux[T, F, IFO1] = new UniformPolyMap[T, F]{
    type Out = IFO1
    def apply(t:T, f:F): Out = {
      val r: MO1 = m(l(t), f)
      //safe
      _if(r.asInstanceOf[Inl[V1, CNil]].head, r)
    }
  }
}
