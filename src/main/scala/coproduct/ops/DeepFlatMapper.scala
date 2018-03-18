package coproduct.ops

import shapeless.Coproduct

//applies MonoMapper and then flattens the result
trait DeepFlatMapper[C, S, D, V] {
  type Out

  def apply(u: C, f: S => D): Out
}

object DeepFlatMapper {
  type Aux[C, S, D, O, V] = DeepFlatMapper[C, S, D, V] {type Out = O}

  implicit def fm[C, S, D, MO <: Coproduct, FO<: Coproduct, V](
                                                                implicit map: MonoMap.Aux[C, S, D, MO, V], flatten: DeepFlatten.Aux[MO, FO]
                                                  ): Aux[C, S, D, FO, V] = {
    new DeepFlatMapper[C, S, D, V] {
      type Out = FO

      def apply(u: C, f: S => D): Out = flatten(map(u, f))
    }
  }
}

