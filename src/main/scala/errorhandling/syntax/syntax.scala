package errorhandling

import cats.syntax.either._
import errorhandling.coproduct.ops.{LiftCp, _}
import errorhandling.misc.boolOps._
import shapeless.ops.coproduct.{Basis, Remove}
import shapeless.{Coproduct, Poly1}

package object syntax {

  object liftCpPoly extends Poly1 {
    implicit def f[T](implicit lc: LiftCp[T]) = at[T](lc(_))
  }
  type Flatten[C <: Coproduct] = shapeless.ops.coproduct.FlatMap[C, liftCpPoly.type]
  type FlattenAux[C <: Coproduct, O <: Coproduct] = shapeless.ops.coproduct.FlatMap.Aux[C, liftCpPoly.type, O]

  trait Embed[Err, Super <: Coproduct] extends (Err => Super)

  object Embed {
    implicit def x[Err, Lerr <: Coproduct, Super <: Coproduct](implicit lcp: LiftCp.Aux[Err, Lerr],
                                                               e: Basis[Super, Lerr]): Embed[Err, Super] =
      v => e.inverse(Right(lcp(v)))
  }

  class RecoverSyntax[V, Err, Res, VT <: VarianceType](e: Either[Err, Res]) {
    def apply[Rest <: Coproduct, RR >: Res, O, IFO](f: V => RR)(
        implicit extract: Extract.Aux[Err, V, Rest, O, VT],
        _if: IF.Aux[O =:= Either[Rest, V], O =:= V, Either[Rest, RR], RR, IFO]
    ): IFO =
      _if(
        evT =>
          e.fold(
            l => evT(extract(l)).map(f),
            r => Right(r)
        ),
        evF => e.valueOr(v => f(evF(extract(v)))),
      )

  }

  // Used only in EitherSyntax.hasError method
  // hasError is single type parameter and no value parameters function, it is not possible to use 'partiallyApplied'syntax to ensure that Res is extands to some type that extends coproduct
  // TODO: think of simpler approach
  trait HasErr[C, E] {
    def apply(c: C): Boolean
  }
  object HasErr {
    implicit def inst[C <: Coproduct, E](implicit rm: Remove[C, E]): HasErr[C, E] = rm(_).isLeft
  }

  object all extends CoproductSyntax with EitherSyntax with EitherTSyntax
}
