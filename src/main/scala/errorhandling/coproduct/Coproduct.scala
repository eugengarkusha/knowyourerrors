package errorhandling.coproduct

import shapeless.CNil

object Coproduct {
  type +:[L, R <: shapeless.Coproduct] = shapeless.:+:[L, R]
  type :+:[L, R] = L +: R +: CNil
}
