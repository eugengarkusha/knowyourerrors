package errorhandling.misc

object boolOps {

  trait IF[TCond, FCond, T, F] {
    type Out
    def apply(t: TCond => T, f: FCond => F): Out
  }
  trait lo {
    implicit def _false[TCond, FCond, T, F](implicit fc: FCond): IF.Aux[TCond, FCond, T, F, F] =
      new IF[TCond, FCond, T, F] {
        type Out = F
        def apply(t: TCond => T, f: FCond => F): Out = f(fc)
      }
  }
  object IF extends lo {
    type Aux[TCond, FCond, T, F, O] = IF[TCond, FCond, T, F] { type Out = O }

    implicit def _true[TCond, FCond, T, F](implicit c: TCond): IF.Aux[TCond, FCond, T, F, T] =
      new IF[TCond, FCond, T, F] {
        type Out = T
        def apply(t: TCond => T, f: FCond => F): Out = t(c)
      }
  }

}
