import coproduct.ops.{Dedup, DeepFlatten}
import shapeless.Coproduct

package object errorhandling {
  // Type level op to
  trait FlattenDedup[C<: Coproduct]{
    type Out<: Coproduct
  }
  object FlattenDedup{
    def apply[C<: Coproduct](implicit fd: FlattenDedup[C]):FlattenDedup.Aux[C, fd.Out] = fd
    type Aux[C<: Coproduct,  O<: Coproduct] = FlattenDedup[C]{type Out = O}
    implicit def inst[C<: Coproduct,  DO<: Coproduct,O<: Coproduct](implicit f: DeepFlatten.Aux[C, DO], dd: Dedup.Aux[DO, O]): Aux[C, O] = null
  }

}
