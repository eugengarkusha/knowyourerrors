package errorhandling


import org.scalatest.Matchers
import org.scalatest.WordSpec
import shapeless.{Inl, Inr}
import errorhandling.coproduct.Coproduct._
import utils._

class UtilsTest extends WordSpec with Matchers {


  "alternatives with If" in {

    val iff01: Either[String, Int] = {
      If(false)(1).right("otherValue")
    }
    iff01 should be(Left("otherValue"))

    val iff02: Either[Int, String] = {
      If(false)(1).left("otherValue")
    }
    iff02 shouldBe Right("otherValue")

    val iff03: Either[Int, String] = {
      If(true)(1).left("otherValue")
    }

    iff03  shouldBe Left(1)

    val iff04: Either[String, Int] = {
      If(true)(1).right("otherValue")
    }

    iff04 shouldBe Right(1)

    val elseIf1: Option[Int :+: String] = {
      If(false)(1)._else(If(true)("2")).opt
    }
    elseIf1 shouldBe Some(Inr(Inl("2")))

    val elseIf2: Option[Int :+: String] = {
      If(false)(1).elseIf(true)("2").opt
    }
    elseIf2 shouldBe elseIf1

    val elseIf3: Option[Int :+: String] = {
      If(true)(1)._else(If(true)("2")).opt
    }
    elseIf3 shouldBe Some(Inl(1))

    val elseIf4: Option[Int :+: String] = {
      If(true)(1).elseIf(true)("2").opt
    }
    elseIf4 shouldBe elseIf3

    val _else1: Int:+: Long = {
      If(false)(1)._else(3L)
    }
    _else1 shouldBe Inr(Inl(3L))

    val _else2: Int :+: Long = {
      If(true)(1)._else(3L)
    }
    _else2 shouldBe Inl(1)

    val none1: Option[Int :+: Long] = {
      If(false)(1).elseIf(false)(2L).opt
    }
    none1 shouldBe None

    val none2: Option[Int :+: Long] = {
      If(false)(1)._else(If(false)(2L)).opt
    }
    none2 shouldBe None

  }

}
