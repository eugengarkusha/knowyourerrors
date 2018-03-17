package coproduct.ops

//typelevel fucntions combinator
trait AndThen[I, A[_, _], B[_, _]] {
  type J
  type Out
  val a: A[I, J]
  val b: B[J, Out]
}

object AndThen {

  //used protected because compiler says "private type Aux1 is never used", which is wrong.
  protected type Aux1[I, A[_, _], JJ, B[_, _], O] = AndThen[I, A, B] {
    type J = JJ
    type Out = O
  }

  type Aux[I, A[_, _], B[_, _], O] = AndThen[I, A, B] {
    type Out = O
  }

  implicit def x[I, O, A[_, _], B[_, _], JJ](implicit _a: A[I, JJ], _b: B[JJ, O]): Aux1[I, A, JJ, B, O] = new AndThen[I, A, B] {
    type J = JJ
    type Out = O
    val a = _a
    val b = _b
  }
}

//workaround for scala bug that leads to limited usability of type aliases during type inference
trait AndThen32[I,I1, A[_, _, _], B[_, _]] {
  type J
  type Out
  val a: A[I,I1, J]
  val b: B[J, Out]
}

object AndThen32 {

  //used protected because compiler says "private type Aux1 is never used", which is wrong.
  protected type Aux1[I,I1, A[_, _, _], JJ, B[_, _], O] = AndThen32[I, I1, A, B] {
    type J = JJ
    type Out = O
  }

  type Aux[I, I1, A[_, _, _], B[_, _], O] = AndThen32[I, I1, A, B] {
    type Out = O
  }

  implicit def instance[I, I1,O, A[_, _,_], B[_, _], JJ](implicit _a: A[I,I1, JJ], _b: B[JJ, O]): Aux1[I,I1, A, JJ, B, O] = new AndThen32[I, I1,A, B] {
    type J = JJ
    type Out = O
    val a = _a
    val b = _b
  }
}