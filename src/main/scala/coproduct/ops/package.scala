package coproduct

import coproduct.ops.MatchSyntax.{Case, ExtractSyntax}
import coproduct.Coproduct._
import misc.boolOps._
import shapeless.ops.coproduct.{Prepend, Remove, Selector}
import shapeless.{CNil, Coproduct, Poly1}

package object ops {


  object liftCpPoly extends Poly1 {
    implicit def f[T](implicit lc: LiftCp[T]) = at[T](lc(_))
  }

  type Flatten[C<: Coproduct] = shapeless.ops.coproduct.FlatMap[C, liftCpPoly.type]
  type FlattenAux[C<: Coproduct, O<: Coproduct] = shapeless.ops.coproduct.FlatMap.Aux[C,liftCpPoly.type , O]

  trait FlattenDedupType[C<: Coproduct]{
    type Out<: Coproduct
  }
  object FlattenDedupType{
    type Aux[C<: Coproduct,  O<: Coproduct] = FlattenDedupType[C]{type Out = O}
    implicit def inst[C<: Coproduct,  DO<: Coproduct,O<: Coproduct](implicit f: FlattenAux[C, DO], dd: Dedup.Aux[DO, O]): Aux[C, O] = null
  }

  def flattenDedupType[C<: Coproduct](implicit fd: FlattenDedupType[C]):FlattenDedupType.Aux[C, fd.Out] = fd


  object If {
    def apply[T](cond: Boolean)(v: => T): If[T] = if(cond) new IfTrue(v) else new IfFalse[T]
  }

  trait If[T] {
    def map[V](f: T => V): If[V]
    def _else[X, T1<: Coproduct, X1<: Coproduct, O<: Coproduct](_if: => If[X])(implicit lt: LiftCp.Aux[T, T1], lx: LiftCp.Aux[X, X1], m: Prepend.Aux[T1, X1, O]): If[O]
    def elseIf[X, T1<: Coproduct, X1<: Coproduct, O<: Coproduct](cond1: Boolean)(v1: => X)(implicit lt: LiftCp.Aux[T, T1], lx: LiftCp.Aux[X, X1], m: Prepend.Aux[T1, X1, O]): If[O]
    def _else[X, T1<: Coproduct, X1<: Coproduct, O<: Coproduct](other: => X)(implicit lt: LiftCp.Aux[T, T1], lx: LiftCp.Aux[X, X1], m: Prepend.Aux[T1, X1, O]): O

    def opt: Option[T]
    def left[R](r: => R): Either[T, R]
    def right[L](l: => L): Either[L, T]
  }

  class IfTrue[T](v: => T) extends If[T] {
    override def map[V](f: T => V): If[V] = new IfTrue[V](f(v))
    override def _else[X, T1<: Coproduct, X1<: Coproduct, O<: Coproduct](not_used: => If[X])(implicit lt: LiftCp.Aux[T, T1], lx: LiftCp.Aux[X, X1], m: Prepend.Aux[T1, X1, O]): If[O] = {
      map(t => m(Left(lt(t))))
    }
    override def elseIf[X, T1<: Coproduct, X1<: Coproduct, O<: Coproduct](cond1: Boolean)(v1: => X)(implicit lt: LiftCp.Aux[T, T1], lx: LiftCp.Aux[X, X1], m: Prepend.Aux[T1, X1, O]): If[O] = {
      _else("this should be never invoked".asInstanceOf[If[X]])
    }
    override def _else[X, T1<: Coproduct, X1<: Coproduct, O<: Coproduct](not_used: => X)(implicit lt: LiftCp.Aux[T, T1], lx: LiftCp.Aux[X, X1], m: Prepend.Aux[T1, X1, O]): O = {
      m(Left(lt(v)))
    }
    def opt: Option[T] = Some(v)
    def left[R](r: => R): Either[T, R] = Left(v)
    def right[L](l: => L): Either[L, T] = Right(v)
  }

  class IfFalse[T] extends If[T] {
    override def map[V](f: T => V): If[V] = this.asInstanceOf[If[V]]
    override def _else[X, T1<: Coproduct, X1<: Coproduct, O<: Coproduct](_if: => If[X])(implicit lt: LiftCp.Aux[T, T1], lx: LiftCp.Aux[X, X1], m: Prepend.Aux[T1, X1, O]): If[O] = {
      _if.map(x => m(Right(lx(x))))
    }
    override def elseIf[X, T1<: Coproduct, X1<: Coproduct, O<: Coproduct](cond1: Boolean)(v1: => X)(implicit lt: LiftCp.Aux[T, T1], lx: LiftCp.Aux[X, X1], m: Prepend.Aux[T1, X1, O]): If[O] = {
      If(cond1)(m(Right(lx(v1))))
    }
    override def _else[X, T1<: Coproduct, X1<: Coproduct, O<: Coproduct](other: => X)(implicit lt: LiftCp.Aux[T, T1], lx: LiftCp.Aux[X, X1], m: Prepend.Aux[T1, X1, O]): O = {
      m(Right(lx(other)))
    }
    def opt: Option[T] = None
    def left[R](r: => R): Either[T, R] = Right(r)
    def right[L](l: => L): Either[L, T] = Left(l)
  }


  object MatchSyntax {
    //TODO: Think of getting rid of notion of emptiness(currently user just unable to unintentionally create empty coproduct)
    def emptyErr: Nothing = throw new Exception("Empty coproduct is used for error handling.")

    class ExtractSyntax[C1, V, Res, VT <: VarianceType](cr: Either[C1, Res]) {
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
      def _case[T]: ExtractSyntax[C1, T, Res, Invariant] = new ExtractSyntax[C1, T, Res, Invariant](rc)
      def _caseAll[T]: ExtractSyntax[C1, T, Res, Covariant] = new ExtractSyntax[C1, T, Res, Covariant](rc)
      def suspend[L, R <: Coproduct, O](
        implicit ev: C1 <:< (L +: R), _if: IF.Aux[R =:= CNil, L, C1, O]
      ): Either[O, Res] = {
        rc.left.map(err => _if(ev.apply(err).left.getOrElse(emptyErr), err))
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

    class CaseSyntax[T] {
      def apply[Res, Extd <: Coproduct, O](f: T => Res)
        (implicit e: Extract.Aux[C, T, Extd, Invariant], _if: IF.Aux[Extd =:= CNil, Res, Case[Extd, Res], O]): O = {
        new ExtractSyntax[C, T, Res, Invariant](Left(or)).apply(f)(e, _if)
      }
    }

    def _case[T]: CaseSyntax[T] = new CaseSyntax[T]

    class CaseAllSyntax[UB] {
      def apply[Res, EO <: Coproduct, O](f: (UB) => Res)
        (implicit e: ExtractCovariant.Aux[C, UB, EO], _if: IF.Aux[EO =:= CNil, Res, Case[EO, Res], O]): O = {
        new ExtractSyntax[C, UB, Res, Covariant](Left(or)).apply(f)(e, _if)
      }
    }

    def _caseAll[UB]: CaseAllSyntax[UB] = new CaseAllSyntax[UB]
  }

}
