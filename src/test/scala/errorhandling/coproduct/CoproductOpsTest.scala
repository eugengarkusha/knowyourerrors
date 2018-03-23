package errorhandling.coproduct

import java.io

import errorhandling.coproduct.ops.Add
import org.scalatest.Matchers
import org.scalatest.WordSpec
import Coproduct._
import shapeless.{CNil, Inl, Inr}
import shapeless.syntax.inject._
import errorhandling.syntax.all._
import errorhandling.utils._


class CoproductOpsTest extends WordSpec with Matchers {

  val intVal: String +: Int :+: Short = 1.inject[String +: Int :+: Short]

  trait SuperXY
  case object X extends SuperXY
  case object Y extends SuperXY

  "adding a value" in {
    (Add(1).to[ String :+: Boolean] : Int +: String :+: Boolean) should be(Inl(1))
    (Add(1).to[Int +: String :+: Boolean]: Int +: String :+: Boolean) should be(Inl(1))
  }


  "extracting values (covariant)" in {

    type alltype = Y.type +: Int +: X.type :+: SuperXY

    val intInjected = 1.inject[alltype]

    (intInjected.extractAll[SuperXY]:  Either[Int +: CNil, SuperXY]) should be(Left(Inl(1)))

    """intInjected.getAll[Byte]""" shouldNot typeCheck

    (X.inject[alltype].extractAll[SuperXY]: Either[Int +: CNil, SuperXY]) should be(Right(X))

    (intInjected.extractAll[Any]: Any) should be(1)

  }

  "getting the least upper bound value" in {
    val x = X.inject[X.type :+: Y.type]

    val lubRes: SuperXY = x.lub
    lubRes should be(X)
  }

  "covariant mapping" in {
    val v = X.inject[String +: X.type :+: Y.type]
    (v.mapC[SuperXY](_ => 1): String +: Int :+: Int) should be(Inr(Inl(1)))
  }

  "invariat mapping" in {
    val v: String +: Int +: Short :+: Int = Inr(Inl(1))
    (v.mapI[Int](_ == 1): String +: Boolean +: Short :+: Boolean) should be(Inr(Inl(true)))
  }

  "covariant flatMapping" in {
    val v: String +: X.type :+: Y.type = Inr(Inl(X))
    (v.flatMapC[SuperXY](_.toString.inject[String :+: Int]).dedup : String :+: Int) should be(Inl("X"))
  }

  "invariant flatMapping" in {
    type CP = String :+: Byte
    def i2CP(i: Int): CP = {
      if (i > 0) {12.toByte.inject[CP] }
      else {"blah".inject[CP] }
    }

    (intVal.flatMapI[Int](i2CP).dedup :String +: Byte :+: Short)  should be(Inr(Inl(12)))

    (intVal.mapI[Int](i2CP).flatten.dedup: String +: Byte :+: Short) should be(Inr(Inl(12)))

    (intVal.flatMapI[Int](v => i2CP(v - 100)).dedup : String +: Byte :+: Short) should be(Inl("blah"))

    (intVal.flatMapI[Int](_.toString.inject[String +: CNil]).dedup : String :+: Short) should be(Inl("1"))
  }

  "flattening" in {

    val nested = Add(1).to[(Byte +: Int :+: Int) +: String +: Int :+: (Short :+: Int)]

    (nested.flatten.dedup: Byte +: String +: Short :+: Int)  should be(Inr(Inr(Inr(Inl(1)))))

    val ft = flattenDedupType[(Byte +: Int +:(Int:+:String)) +: String +: Int +: (Short :+: Int) :+: Double]
    implicitly[=:=[ft.Out, Byte +: String +: Short +: Int :+: Double]]
  }


  "case syntax" in {

    trait Err
    case object Err1 extends Err
    case object Err2 extends Err
    case object Err3 extends Err


    (Err1.inject[Err1.type +: Err2.type :+: Err3.type]
        .recoverFrom[Err1.type](_.toString)
        .recoverFrom[Err3.type](_.toString)
        .recoverFrom[Err2.type](_.toString): String) should be("Err1")


    (Err2.inject[Err1.type +: Err2.type :+: Err3.type]
        .recoverFrom[Err1.type](_.toString)
        .recoverFromAll[Err](_.toString): String) should be("Err2")


    (Err2.inject[Err1.type +: Err2.type :+: Err3.type]
        //Return type is stated explicitly because of the known inference problem
        .recoverFrom[Err1.type](_ => (throw new RuntimeException("")): Int).left.get : Err2.type :+: Err3.type
      ) should be(Inl(Err2))


    """val process3: String = {
      Inject(Err2).into[Err1.type +: Err2.type :+: Err3.type]
      .recoverFrom[Err1.type ](_.toString)
      .recoverFrom[Err2.type](_.toString)
    }""" shouldNot typeCheck

  }

  "caseAll, caseOthers, suspend" in {

    trait GenError
    case class ValidationErr(msg: String) extends GenError
    trait ServiceErr extends GenError
    case class IOErr(msg: String) extends ServiceErr
    trait TimeOut extends ServiceErr

    type commonSuper = ValidationErr +: IOErr :+: TimeOut

    val valErr = ValidationErr("err").inject[commonSuper]
    val ioErr = IOErr("boom").inject[commonSuper]

   
    valErr.recoverFromAll[GenError](_.toString) should be("ValidationErr(err)")

    (valErr.recoverFromAll[ServiceErr](_.toString).left.map(_.lub) : Either[ValidationErr, String]) should be(
      Left(ValidationErr("err"))
    )

    (ioErr.recoverFrom[TimeOut](_.toString): Either[ValidationErr :+: IOErr, String]) should be (
      Left(Inr(Inl(IOErr("boom"))))
    )

    (ioErr.recoverFrom[IOErr](_.toString).recoverFromAll[GenError](_.toString + "!!!"): String) should be ("IOErr(boom)")

    (ioErr.recoverFrom[IOErr]("Recovered:" + _): Either[ValidationErr :+: TimeOut, String]) should be (
      Right("Recovered:IOErr(boom)")
    )

  }
}
