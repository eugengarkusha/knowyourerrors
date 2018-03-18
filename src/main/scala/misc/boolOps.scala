package misc

object boolOps {

  trait IF[Cond, T, F] {
    type Out
    def apply(l: => T, r: => F): Out
  }
  trait lo {
    implicit def _false[C, T, F]: IF.Aux[C, T, F, F] = new IF[C, T, F] {
      type Out = F
      def apply(l: => T, r: => F): Out = r
    }
  }
  object IF extends lo {
    type Aux[C, T, F, O] = IF[C, T, F] {type Out = O}

    implicit def _true[C, T, F](implicit c: C): IF.Aux[C, T, F, T] = new IF[C, T, F] {
      type Out = T
      def apply(l: => T, r: => F): Out = l
    }
  }

}
