package coproduct.ops

import shapeless.Coproduct

//Reuses Mapper and then flattens the result
trait FlatMapper[C, S, D, V] {
  type Out

  def apply(u: C, f: S => D): Out
}

object FlatMapper {
  type Aux[C, S, D, O, V] = FlatMapper[C, S, D, V] {type Out = O}

  implicit def fm[C, S, D, MO <: Coproduct, FO<: Coproduct, V](
                                                    implicit map: MapOne.Aux[C, S, D, MO, V], flatten: Flatten.Aux[MO, FO]
                                                  ): Aux[C, S, D, FO, V] = {
    new FlatMapper[C, S, D, V] {
      type Out = FO

      def apply(u: C, f: S => D): Out = flatten(map(u, f))
    }
  }
}

