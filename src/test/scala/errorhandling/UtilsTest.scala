package errorhandling


import org.scalatest.Matchers
import org.scalatest.WordSpec

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}
import errorhandling.coproduct.ops._
import shapeless.{CNil, Inl, Inr}
import errorhandling.coproduct.Coproduct._
import syntax._
import cats.data.EitherT
import cats.instances.future._
import shapeless.syntax.inject._
import utils._


class UtilsTest extends WordSpec with Matchers {

  implicit class FutureSyntax[T](f: Future[T]) {
    import scala.concurrent.duration._
    def await() = Await.result(f, 10 seconds)
  }


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

    val elseIf1: Option[+:[Int, +:[String, CNil]]] = {
      If(false)(1)._else(If(true)("2")).opt
    }
    elseIf1 shouldBe Some(Inr(Inl("2")))

    val elseIf2: Option[+:[Int, +:[String, CNil]]] = {
      If(false)(1).elseIf(true)("2").opt
    }
    elseIf2 shouldBe elseIf1

    val elseIf3: Option[+:[Int, +:[String, CNil]]] = {
      If(true)(1)._else(If(true)("2")).opt
    }
    elseIf3 shouldBe Some(Inl(1))

    val elseIf4: Option[+:[Int, +:[String, CNil]]] = {
      If(true)(1).elseIf(true)("2").opt
    }
    elseIf4 shouldBe elseIf3

    val _else1: +:[Int, +:[Long, CNil]] = {
      If(false)(1)._else(3L)
    }
    _else1 shouldBe Inr(Inl(3L))

    val _else2: +:[Int, +:[Long, CNil]] = {
      If(true)(1)._else(3L)
    }
    _else2 shouldBe Inl(1)

    val none1: Option[+:[Int, +:[Long, CNil]]] = {
      If(false)(1).elseIf(false)(2L).opt
    }
    none1 shouldBe None

    val none2: Option[+:[Int, +:[Long, CNil]]] = {
      If(false)(1)._else(If(false)(2L)).opt
    }
    none2 shouldBe None

  }


  "Eitehr syntax" in {

    val r = Right[Int, String]("s")
    val l = Left[Int, String](1)

    type common = Int +: String :+: Short


    val embedded: Either[common, String] = r.embed[common]
    embedded shouldBe Right("s")

    val embeddedL: Either[common, String] = l.embed[common]
    embeddedL shouldBe Left(Inl(1))

    val partHandled = embedded.partiallyHandle(_._case[Int](_.toString))
    partHandled shouldBe Right("s")

    val PartHandledL: Either[String :+: Short, String] = embeddedL.partiallyHandle(_._case[Int](_.toString))
    PartHandledL shouldBe Right("1")

    //removing CNil at the last step
    val js1: Either[Short, String] = Left(42.inject[Int :+: Short]).partiallyHandle(_._case[Int](_.toString))
    js1 shouldBe(Right("42"))

    val handled: String = embedded.handle(_ => "err")
    handled shouldBe "s"

    val handledL: String = embeddedL.handle(_ => "err")
    handledL shouldBe "err"

    Left(1.inject[common]).hasError[Int] shouldBe true
    Left("blah".inject[common]).hasError[Int] shouldBe false
    Right[common, Int](42).hasError[Int] shouldBe false

    val tobeJoined: Either[Long :+: String, Either[Unit :+: Char, Symbol]] = {
      If(true)(1L).elseIf(true)("2").left(If(false)(()).elseIf(true)('a').left('x))
    }
    val joined: Either[Long +: String +: Unit :+: Char, Symbol] = tobeJoined.joinCP

    joined shouldBe (Left(Inl(1)))

    val tobeJoinedWithDuplicates: Either[Long :+: String, Either[Unit :+: Long, Symbol]] = {
      If(true)(1L).elseIf(true)("2").left(If(false)(()).elseIf(true)(2L).left('x))
    }
    //joinCP just merges the coproducts, if flattening is needed - do left.map(_.flatten)
    //currently flattening is much more expensive then just merge so not performed by default
    val joinedWithDuplicates: Either[Long +: String +: Unit :+: Long, Symbol] = tobeJoinedWithDuplicates.joinCP
    joinedWithDuplicates shouldBe (Left(Inl(1)))

    val resNoDup: Either[String +: Unit :+: Long, Symbol] = joinedWithDuplicates.left.map(_.dedup)
    resNoDup shouldBe(Left(Inr(Inr(Inl(1L)))))

    val ensured: Either[String :+: Int, String] = r.ensureCP("fail")(_ != "s")
    ensured should be (Left(Inl("fail")))

    def e1: Either[Int, String] = Left(1)
    def e2: Either[Byte, String] = Right("ss")
    def e3: Either[Long :+: Short, String] = Right("ss1")

    val or: Either[+:[Int, +:[Byte, :+:[Long, Short]]], String] = e1.or(e2).or(e3)
    val or1: Either[+:[Int, +:[Byte, :+:[Long, Short]]], String] = e1.or(e2.or(e3))
    val or2: Either[+:[Long, +:[Short, +:[Int, +:[Byte, CNil]]]], String] = e3 or e1 or e2
    or should be(Right("ss"))
    or1 should be(or)
    or2 should be(Right("ss1"))
  }

  "EitherTSyntax" in {


    type ETV[E, V] = EitherT[Future, E, V]
    type ET[E] = EitherT[Future, E, String]

    val fmt: ET[Int] = EitherT.liftF(Future.successful("s"))
    val fmtErr: ET[Int] = EitherT.fromEither(Left(400))

    val ensured: ET[String :+: Int] = fmt.ensureCP("some err")(_ != "s")
    ensured.value.await() shouldBe Left(Inl("some err"))

    val embedded: ET[Int :+: String] = fmtErr.embed[Int :+: String]
    embedded.value.await() shouldBe Left(Inl(400))

    val handled: Future[String] = embedded.handle(
      _._case[Int]("Recovered:"+_)
        ._case[String]("Recovered:"+_)
    )
    handled.await() shouldBe ("Recovered:400")

    val partHandled: ET[Int] = embedded.partiallyHandle(_._case[String]("Recovered:"+_))
    partHandled.value.await() shouldBe(Left(400))

  }

}
