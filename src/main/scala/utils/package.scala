import coproduct.Coproduct.+:
import coproduct.ops.{Covariant, Dedup, Extract, FlattenAux, Invariant, LiftCp, VarianceType}
import misc.boolOps.IF
import shapeless.{CNil, Coproduct}
import shapeless.ops.coproduct.Prepend

package object utils {

  trait FlattenDedupType[C <: Coproduct] {
    type Out <: Coproduct
  }
  object FlattenDedupType {
    type Aux[C <: Coproduct, O <: Coproduct] = FlattenDedupType[C] { type Out = O }
    implicit def inst[C <: Coproduct, DO <: Coproduct, O <: Coproduct](implicit f: FlattenAux[C, DO],
                                                                       dd: Dedup.Aux[DO, O]): Aux[C, O] = null
  }

  def flattenDedupType[C <: Coproduct](implicit fd: FlattenDedupType[C]): FlattenDedupType.Aux[C, fd.Out] = fd

  object If {
    def apply[T](cond: Boolean)(v: => T): If[T] = if (cond) new IfTrue(v) else new IfFalse[T]
  }

  trait If[T] {
    def map[V](f: T => V): If[V]
    def _else[X, T1 <: Coproduct, X1 <: Coproduct, O <: Coproduct](
        _if: => If[X])(implicit lt: LiftCp.Aux[T, T1], lx: LiftCp.Aux[X, X1], m: Prepend.Aux[T1, X1, O]): If[O]
    def elseIf[X, T1 <: Coproduct, X1 <: Coproduct, O <: Coproduct](cond1: Boolean)(
        v1: => X)(implicit lt: LiftCp.Aux[T, T1], lx: LiftCp.Aux[X, X1], m: Prepend.Aux[T1, X1, O]): If[O]
    def _else[X, T1 <: Coproduct, X1 <: Coproduct, O <: Coproduct](
        other: => X)(implicit lt: LiftCp.Aux[T, T1], lx: LiftCp.Aux[X, X1], m: Prepend.Aux[T1, X1, O]): O

    def opt: Option[T]
    def left[R](r: => R): Either[T, R]
    def right[L](l: => L): Either[L, T]
  }

  class IfTrue[T](v: => T) extends If[T] {
    override def map[V](f: T => V): If[V] = new IfTrue[V](f(v))
    override def _else[X, T1 <: Coproduct, X1 <: Coproduct, O <: Coproduct](
        not_used: => If[X])(implicit lt: LiftCp.Aux[T, T1], lx: LiftCp.Aux[X, X1], m: Prepend.Aux[T1, X1, O]): If[O] = {
      map(t => m(Left(lt(t))))
    }
    override def elseIf[X, T1 <: Coproduct, X1 <: Coproduct, O <: Coproduct](cond1: Boolean)(
        v1: => X)(implicit lt: LiftCp.Aux[T, T1], lx: LiftCp.Aux[X, X1], m: Prepend.Aux[T1, X1, O]): If[O] = {
      _else("this should be never invoked".asInstanceOf[If[X]])
    }
    override def _else[X, T1 <: Coproduct, X1 <: Coproduct, O <: Coproduct](
        not_used: => X)(implicit lt: LiftCp.Aux[T, T1], lx: LiftCp.Aux[X, X1], m: Prepend.Aux[T1, X1, O]): O = {
      m(Left(lt(v)))
    }
    def opt: Option[T] = Some(v)
    def left[R](r: => R): Either[T, R] = Left(v)
    def right[L](l: => L): Either[L, T] = Right(v)
  }

  class IfFalse[T] extends If[T] {
    override def map[V](f: T => V): If[V] = this.asInstanceOf[If[V]]
    override def _else[X, T1 <: Coproduct, X1 <: Coproduct, O <: Coproduct](
        _if: => If[X])(implicit lt: LiftCp.Aux[T, T1], lx: LiftCp.Aux[X, X1], m: Prepend.Aux[T1, X1, O]): If[O] = {
      _if.map(x => m(Right(lx(x))))
    }
    override def elseIf[X, T1 <: Coproduct, X1 <: Coproduct, O <: Coproduct](cond1: Boolean)(
        v1: => X)(implicit lt: LiftCp.Aux[T, T1], lx: LiftCp.Aux[X, X1], m: Prepend.Aux[T1, X1, O]): If[O] = {
      If(cond1)(m(Right(lx(v1))))
    }
    override def _else[X, T1 <: Coproduct, X1 <: Coproduct, O <: Coproduct](
        other: => X)(implicit lt: LiftCp.Aux[T, T1], lx: LiftCp.Aux[X, X1], m: Prepend.Aux[T1, X1, O]): O = {
      m(Right(lx(other)))
    }
    def opt: Option[T] = None
    def left[R](r: => R): Either[T, R] = Right(r)
    def right[L](l: => L): Either[L, T] = Left(l)
  }
}
