package errorhandling.syntax

import errorhandling.coproduct.Coproduct.+:
import errorhandling.coproduct.ops._
import shapeless.{Coproduct, Poly1}
import shapeless.ops.coproduct.Selector

trait CoproductSyntax {

  implicit class CoproductOps[L, R <: Coproduct](or: L +: R) {

    type C = L +: R

    def flatten[O <: Coproduct](implicit fl: FlattenAux[C, O]): O = fl(or)

    def dedup[O](implicit dd: Dedup.Aux[C, O]): O = dd(or)

    //extract the value from this coproduct as the least uper bound of all types
    def lub[O](implicit get: Selector[C, O]): O = get(or).ensuring(_.isDefined).get

    def extractAll[V](implicit extract: Extract[C, V, Covariant]): extract.Out = extract(or)

    def extract[V](implicit extract: Extract[C, V, Invariant]): extract.Out = extract(or)

    class MapSyntax[S, V <: VarianceType] {
      def apply[D](f: S => D)(implicit mapper: MonoMap[C, S, D, V]): mapper.Out = mapper(or, f)
    }

    def mapI[S]: MapSyntax[S, Invariant] = new MapSyntax[S, Invariant]

    def mapC[S]: MapSyntax[S, Covariant] = new MapSyntax[S, Covariant]

    class FlatMapSyntax[S, V <: VarianceType] {
      def apply[D <: Coproduct, MO <: Coproduct, O <: Coproduct](f: S => D)(
          implicit m: MonoMap.Aux[C, S, D, MO, V],
          df: FlattenAux[MO, O]
      ): O = df(m(or, f))
    }

    def flatMapI[S]: FlatMapSyntax[S, Invariant] = new FlatMapSyntax[S, Invariant]

    def flatMapC[S]: FlatMapSyntax[S, Covariant] = new FlatMapSyntax[S, Covariant]

    def recoverFrom[V] = new RecoverSyntax[V, C, Nothing, Invariant](Left(or))
    def recoverFromAll[V] = new RecoverSyntax[V, C, Nothing, Covariant](Left(or))

  }
}
