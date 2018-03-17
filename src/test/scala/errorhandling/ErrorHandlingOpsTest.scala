package errorhandling


import org.scalatest.Matchers
import org.scalatest.WordSpec

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}
import scala.util.Failure
import scala.util.Success
import scala.util.Try
import shapeless.{Inl, Inr, CNil}
import _root_.coproduct.ops._
import _root_.coproduct.Coproduct._
import _root_.ops.ops._
import _root_.ops.EitherOps._
import errors.GenErr

class ErrorHandlingOpsTest extends WordSpec with Matchers {

  implicit class FutureSyntax[T](f: Future[T]) {
    import scala.concurrent.duration._
    def await() = Await.result(f, 10 seconds)
  }
  "alternatives with or" in {
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

  "EitherOps" in {

    val r = Right[Int, String]("s")
    val l = Left[Int, String](1)

    type common = Int +: String :+: Short

    val fmt: FutureEitherMT[Int, String] = r.futMt
    fmt.v.value shouldBe Some(Success(Right("s")))

    val ensured: FutureEitherMT[+:[String, +:[Int, CNil]], String] = fmt.ensureCP("some err")(_ != "s")
    ensured.v.await() shouldBe Left(Inl("some err"))

    val ensured1: FutureEitherMT[Int, String] = fmt.ensure(2)(_ != "s")
    ensured1.v.await() shouldBe Left(2)

    val ensured2: FutureEitherMT[String, Unit] = ensure("some err")(false)
    ensured2.v.await() shouldBe Left("some err")

    val fmta: FutureEitherMT[common, String] = r.futMtA[common]
    fmta.v.value shouldBe Some(Success(Right("s")))

    val fmtaL: FutureEitherMT[common, String] = l.futMtA[common]
    fmtaL.v.value shouldBe Some(Success(Left(Inl(1))))

    val aligned: Either[common, String] = r.align[common]
    aligned shouldBe Right("s")

    val alignedL: Either[common, String] = l.align[common]
    alignedL shouldBe Left(Inl(1))

    val partHandled = aligned.partiallyHandle(_._case[Int](_.toString))
    partHandled shouldBe Right("s")

    val PartHandledL: Either[String :+: Short, String] = alignedL.partiallyHandle(_._case[Int](_.toString))
    PartHandledL shouldBe Right("1")

    //removing CNil at the last step
    val js1: Either[Short, String] = Left(Inject(42).to[Int :+: Short]).partiallyHandle(_._case[Int](_.toString))
    js1 shouldBe(Right("42"))

    val handled: String = aligned.handle(_ => "err")
    handled shouldBe "s"

    val handledL: String = alignedL.handle(_ => "err")
    handledL shouldBe "err"

    Left(Inject(1).to[common]).hasError[Int] shouldBe true
    Left(Inject("blah").to[common]).hasError[Int] shouldBe false
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

    val resNoDup: Either[String +: Unit :+: Long, Symbol] = joinedWithDuplicates.left.map(_.flatten)
    resNoDup shouldBe(Left(Inr(Inr(Inl(1L)))))
  }

  "future ops" in {
    val f = Future.successful(1)

    val right: FutureEitherMT[Nothing, Int] = f.toRight
    right.v.await() shouldBe Right(1)

    val left: FutureEitherMT[Int, Nothing] = f.toLeft
    left.v.await()  shouldBe Left(1)
  }

  "any ops" in {

    val i = 1

    """i.injectTo[String :+: Boolean]""" shouldNot typeCheck

    val injected: String :+: Int = i.injectTo[String :+: Int]
    injected shouldBe Inr(Inl(1))

    val added: Int +: String :+: Boolean = i.addTo[String :+: Boolean]
    added shouldBe Inl(1)

    val left: Either[Int, Byte] = i.left[Byte]
    left shouldBe Left(1)

    val right: Either[Byte, Int] = i.right[Byte]
    right shouldBe Right(1)

  }

  "try ops" in {
    val err = new Exception()
    val f: Try[String] = Failure(err)
    val s: Try[String]= Success("s")

    val es: Either[Throwable, String] = s.asEither
    es shouldBe Right("s")
    val ef: Either[Throwable, String] = f.asEither
    ef shouldBe Left(err)

    val ws: Either[Int, String] = s.wrapWith(_.toString.size)
    ws shouldBe Right("s")

    val wf: Either[Int, String] =f.wrapWith(_.toString.size)
    wf shouldBe(Left(19))

    val ps: Either[GenErr, String] = s.wrapWithErr("err happened")
    ps shouldBe Right("s")

    val pf: Either[GenErr, String] = f.wrapWithErr("err happened")
    pf shouldBe(Left(GenErr("err happened", Some(Right(err)))))
  }
  //TODO: cover other operations

}
