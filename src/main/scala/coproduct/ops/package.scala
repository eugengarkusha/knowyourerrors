package coproduct

import coproduct.ops.MatchSyntax.{Case, ExtractSyntax}
import coproduct.Coproduct._
import misc.boolOps._
import shapeless.{Coproduct, CNil}

package object ops {

  object If {
    def apply[T](cond: Boolean)(v: => T): If[T] = if(cond) new IfTrue(v) else new IfFalse[T]
  }

  trait If[T] {
    def map[V](f: T => V): If[V]
    def _else[X, T1, X1, O](_if: => If[X])(implicit lt: LiftCp.Aux[T, T1], lx: LiftCp.Aux[X, X1], m: Merge.Aux[T1, X1, O]): If[O]
    def elseIf[X, T1, X1, O](cond1: Boolean)(v1: => X)(implicit lt: LiftCp.Aux[T, T1], lx: LiftCp.Aux[X, X1], m: Merge.Aux[T1, X1, O]): If[O]
    def _else[X, T1, X1, O](other: => X)(implicit lt: LiftCp.Aux[T, T1], lx: LiftCp.Aux[X, X1], m: Merge.Aux[T1, X1, O]): O

    def opt: Option[T]
    def left[R](r: => R): Either[T, R]
    def right[L](l: => L): Either[L, T]
  }

  class IfTrue[T](v: => T) extends If[T] {
    override def map[V](f: T => V): If[V] = new IfTrue[V](f(v))
    override def _else[X, T1, X1, O](not_used: => If[X])(implicit lt: LiftCp.Aux[T, T1], lx: LiftCp.Aux[X, X1], m: Merge.Aux[T1, X1, O]): If[O] = {
      map(t => m.append(lt(t)))
    }
    override def elseIf[X, T1, X1, O](cond1: Boolean)(v1: => X)(implicit lt: LiftCp.Aux[T, T1], lx: LiftCp.Aux[X, X1], m: Merge.Aux[T1, X1, O]): If[O] = {
      _else("this should be never invoked".asInstanceOf[If[X]])
    }
    override def _else[X, T1, X1, O](not_used: => X)(implicit lt: LiftCp.Aux[T, T1], lx: LiftCp.Aux[X, X1], m: Merge.Aux[T1, X1, O]): O = {
      m.append(lt(v))
    }
    def opt: Option[T] = Some(v)
    def left[R](r: => R): Either[T, R] = Left(v)
    def right[L](l: => L): Either[L, T] = Right(v)
  }

  class IfFalse[T] extends If[T] {
    override def map[V](f: T => V): If[V] = this.asInstanceOf[If[V]]
    override def _else[X, T1, X1, O](_if: => If[X])(implicit lt: LiftCp.Aux[T, T1], lx: LiftCp.Aux[X, X1], m: Merge.Aux[T1, X1, O]): If[O] = {
      _if.map(x => m.prepend(lx(x)))
    }
    override def elseIf[X, T1, X1, O](cond1: Boolean)(v1: => X)(implicit lt: LiftCp.Aux[T, T1], lx: LiftCp.Aux[X, X1], m: Merge.Aux[T1, X1, O]): If[O] = {
      If(cond1)(m.prepend(lx(v1)))
    }
    override def _else[X, T1, X1, O](other: => X)(implicit lt: LiftCp.Aux[T, T1], lx: LiftCp.Aux[X, X1], m: Merge.Aux[T1, X1, O]): O = {
      m.prepend(lx(other))
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

  //ops for non-empty coproducts(L +: R)
  implicit class CoproductOps[L, R <: Coproduct](or: L +: R) {

    type C = L +: R

    def align[Dst](implicit align: Align[C, Dst]): Dst = align(or)

    def flatten[O](implicit fl: Flatten.Aux[C, O]): O = fl(or)

    //extract the value from this coproduct as the least uper bound of all types
    def lub[O](implicit get: Get.Aux[C, O]): O = {
      get(or).getOrElse(throw new Exception("Empty coproduct found in non-empty coproduct ops. This may be caused by type casting."))
    }

    def extract[T](implicit extract: ExtractInvariant[C, T]): Either[extract.Out, T] = extract(or)

    def extractAll[V](implicit extract: ExtractCovariant[C, V, MatchRequired]): Either[extract.Out, V] = extract(or)

    def contains[T](implicit extract: ExtractInvariant[C, T]): Boolean = extract(or).isRight

    def remove[T](implicit extract: ExtractInvariant[C, T]): Option[extract.Out] = extract(or).fold(Some(_), _ => None)

    class MapSyntax[S, V <: VarianceType] {
      def apply[D](f: S => D)(implicit mapper: Mapper[C, S, D, V]): mapper.Out = mapper(or, f)
    }

    //TODO: create an implementation of PolyFcuntion for and a corresponding mapAll method
    def mapI[S]: MapSyntax[S, Invariant] = new MapSyntax[S, Invariant]

    def mapC[S]: MapSyntax[S, Covariant] = new MapSyntax[S, Covariant]

    class FlatMapSyntax[S, V <: VarianceType] {
      def apply[D](f: S => D)(implicit mapper: FlatMapper[C, S, D, V]): mapper.Out = mapper(or, f)
    }

    def flatMapI[S]: FlatMapSyntax[S, Invariant] = new FlatMapSyntax[S, Invariant]

    def flatMapC[S]: FlatMapSyntax[S, Covariant] = new FlatMapSyntax[S, Covariant]

    def diff[C1 <: Coproduct](implicit d: Diff[C, C1]): Option[d.Out] = d(or)

    def extendWith[T](implicit a: Add[C, T]): a.Out = a.extend(or)

    def append[C1 <: Coproduct](implicit a: AndThen32[L +: R, C1, Merge.Aux, Dedup.Aux]): a.Out = a.b(a.a.append(or))

    def prepend[C1 <: Coproduct](implicit a: AndThen32[C1, L +: R, Merge.Aux, Dedup.Aux]): a.Out = a.b(a.a.prepend(or))

    class CaseSyntax[T] {
      def apply[Res, Extd <: Coproduct, O](f: T => Res)
        (implicit e: ExtractInvariant.Aux[C, T, Extd], _if: IF.Aux[Extd =:= CNil, Res, Case[Extd, Res], O]): O = {
        new ExtractSyntax[C, T, Res, Invariant](Left(or)).apply(f)(e, _if)
      }
    }

    def _case[T]: CaseSyntax[T] = new CaseSyntax[T]

    class CaseAllSyntax[UB] {
      def apply[Res, EO <: Coproduct, O](f: (UB) => Res)
        (implicit e: ExtractCovariant.Aux[C, UB, EO, MatchRequired], _if: IF.Aux[EO =:= CNil, Res, Case[EO, Res], O]): O = {
        new ExtractSyntax[C, UB, Res, Covariant](Left(or)).apply(f)(e, _if)
      }
    }

    def _caseAll[UB]: CaseAllSyntax[UB] = new CaseAllSyntax[UB]
  }

}
