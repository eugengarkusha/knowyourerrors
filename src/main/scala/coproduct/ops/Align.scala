package coproduct.ops

import coproduct.Coproduct.+:
import shapeless.{Coproduct, CNil}

trait Align[Src, Dst]{
  def apply(i: Src): Dst
}

object Align {

  implicit def cnil [CNil, Dst](implicit inject: Add.Aux[Dst, CNil, Dst]): Align[CNil, Dst] = {
    new Align[CNil, Dst]{
      def apply(i: CNil): Dst = inject(i)
    }
  }

  implicit def recurse[L, R <: Coproduct , Dst](implicit inject: Add.Aux[Dst, L, Dst], align: Align[R, Dst]): Align[L +: R, Dst] = {
    new Align[L +: R, Dst]{
      def apply(i: L +: R): Dst = i.eliminate(inject(_), align(_))
    }
  }
}