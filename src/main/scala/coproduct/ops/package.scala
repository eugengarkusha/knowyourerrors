package coproduct

import coproduct.Coproduct._
import coproduct.ops.MatchSyntax
import misc.boolOps._
import shapeless.ops.coproduct.{Prepend, Remove, Selector}
import shapeless.{CNil, Coproduct, Poly1}
import ops.MatchSyntax._

package object ops {


  object liftCpPoly extends Poly1 {
    implicit def f[T](implicit lc: LiftCp[T]) = at[T](lc(_))
  }

  type Flatten[C<: Coproduct] = shapeless.ops.coproduct.FlatMap[C, liftCpPoly.type]
  type FlattenAux[C<: Coproduct, O<: Coproduct] = shapeless.ops.coproduct.FlatMap.Aux[C,liftCpPoly.type , O]

  object MatchSyntax {
    def emptyErr: Nothing = throw new AssertionError("got value of type CNil")

    class CaseSyntax[C1, V, Res, VT <: VarianceType](cr: Either[C1, Res]) {
      def apply[EO <: Coproduct, O, Res1 >: Res](f: V => Res1)(
        implicit extract: Extract.Aux[C1, V, EO, VT], _if: IF.Aux[EO =:= CNil, Res1, Case[EO, Res1], O]
      ): O = {
        cr.fold(
          c1 => extract(c1).fold(
            e0 => _if(emptyErr, new Case(Left(e0))),
            v => _if(f(v), new Case(Right(f(v))))
          ),
          res => _if(res, new Case(Right(res)))
        )
      }
    }

    class Case[C1, Res](rc: Either[C1, Res]) {
      def _case[T]: CaseSyntax[C1, T, Res, Invariant] = new CaseSyntax[C1, T, Res, Invariant](rc)
      def _caseAll[T]: CaseSyntax[C1, T, Res, Covariant] = new CaseSyntax[C1, T, Res, Covariant](rc)
      def suspend[L, R <: Coproduct, O](
                                         implicit ev: C1 <:< (L +: R), _if: IF.Aux[R =:= CNil, L, C1, O]
                                       ): Either[O, Res] = {
        rc.left.map(err => _if(ev(err).left.getOrElse(emptyErr), err))
      }
    }
  }


  implicit class CoproductOps[L, R <: Coproduct](or: L +: R) {

    type C = L +: R

    def flatten[O<: Coproduct](implicit fl: FlattenAux[C, O]): O = fl(or)

    def dedup[O](implicit dd: Dedup.Aux[C,O]):O = dd(or)

    //extract the value from this coproduct as the least uper bound of all types
    def lub[O](implicit get: Selector[C, O]): O = get(or).ensuring(_.isDefined).get

    def extract[T](implicit remove: Remove[C, T]): Either[remove.Rest, T] = remove(or).swap

    def extractAll[V](implicit extract: ExtractCovariant[C, V]): Either[extract.Rest, V] = extract(or)

    class MapSyntax[S, V <: VarianceType] {
      def apply[D](f: S => D)(implicit mapper: MonoMap[C, S, D, V]): mapper.Out = mapper(or, f)
    }

    def mapI[S]: MapSyntax[S, Invariant] = new MapSyntax[S, Invariant]

    def mapC[S]: MapSyntax[S, Covariant] = new MapSyntax[S, Covariant]

    class FlatMapSyntax[S, V <: VarianceType] {
      def apply[D, MO <: Coproduct ,O <: Coproduct](f: S => D)(
        implicit m: MonoMap.Aux[C, S, D, MO, V],
        df: FlattenAux[MO, O]
      ): O = df(m(or, f))
    }

    def flatMapI[S]: FlatMapSyntax[S, Invariant] = new FlatMapSyntax[S, Invariant]

    def flatMapC[S]: FlatMapSyntax[S, Covariant] = new FlatMapSyntax[S, Covariant]

    def extendWith[T](implicit a: Add[C, T]): a.Out = a.extend(or)


    def _case[T]: InitialCaseSyntax[T] = new InitialCaseSyntax[T]

    // TODO: think how to avoid separate initial cases
    class InitialCaseSyntax[T] {
      def apply[R, Rest <: Coproduct, O](f: T => R)
                                        (implicit e: Extract.Aux[C, T, Rest, Invariant], _if: IF.Aux[Rest =:= CNil, R, Case[Rest, R], O]): O = {
        new CaseSyntax[C, T, R, Invariant](Left(or)).apply(f)(e, _if)
      }
    }

    def _caseAll[UB]: InitialCaseAllSyntax[UB] = new InitialCaseAllSyntax[UB]

    class InitialCaseAllSyntax[T] {
      def apply[R, Rest <: Coproduct, O](f: T => R)
                                        (implicit e: ExtractCovariant.Aux[C, T, Rest], _if: IF.Aux[Rest =:= CNil, R, Case[Rest, R], O]): O = {
        new CaseSyntax[C, T, R, Covariant](Left(or)).apply(f)(e, _if)
      }
    }

  }

}
