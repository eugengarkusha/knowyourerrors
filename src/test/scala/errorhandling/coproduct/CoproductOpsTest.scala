package errorhandling.coproduct

import errorhandling.coproduct.ops.Add
import org.scalatest.Matchers
import org.scalatest.WordSpec
import Coproduct._
import ops._
import errorhandling.errors._
import shapeless.{CNil, Inl, Inr}
import shapeless.syntax.inject._
import errorhandling.utils._


class CoproductOpsTest extends WordSpec with Matchers {

  val intVal: String +: Int :+: Short = 1.inject[String +: Int :+: Short]

  trait SuperXY
  case object X extends SuperXY
  case object Y extends SuperXY

  "adding/injecting a value" in {

    type C = String :+: Boolean

    val i: Int +: String :+: Boolean = Add(1).to[C]
    i should be(Inl(1))

    val i1: C = "s".inject[C]
    i1 should be(Inl("s"))

    """1.inject[C]""" shouldNot typeCheck
    """CNil.inject[Int +: CNil]""" shouldNot typeCheck
  }

  "extracting/removing values (invariant)" in {
    val extracted: Either[String :+: Short, Int] = intVal.extract[Int]
    extracted should be(Right(1))

    val extracted1: Either[Int :+: Short, String] = intVal.extract[String]
    extracted1 should be(Left(Inl(1)))

    intVal.extractAll[Any] should be(Right(1))

  }


  "extracting values (covariant)" in {

    type alltype = Y.type +: Int +: X.type :+: SuperXY

    val intInjected = 1.inject[alltype]

    val all: Either[Int +: CNil, SuperXY] = intInjected.extractAll[SuperXY]
    all should be(Left(Inl(1)))

    """intInjected.getAll[Byte]""" shouldNot typeCheck

    val all1: Either[Int +: CNil, SuperXY] = X.inject[alltype].extractAll[SuperXY]
    all1 should be(Right(X))
  }

  "getting the least upper bound value" in {
    val x = X.inject[X.type :+: Y.type]

    val lubRes: SuperXY = x.lub
    lubRes should be(X)
  }

  "covariant mapping" in {
    val v: String +: X.type :+: Y.type = Inr(Inl(X))
    val mapped: String +: Int :+: Int = v.mapC[SuperXY](_ => 1)
    mapped should be(Inr(Inl(1)))
  }

  "invariat mapping" in {
    val v: String +: Int +: Short :+: Int = Inr(Inl(1))
    val mapped: String +: Boolean +: Short :+: Boolean = v.mapI[Int](_ == 1)
    mapped should be(Inr(Inl(true)))
  }

  "covariant flatMapping" in {
    val v: String +: X.type :+: Y.type = Inr(Inl(X))
    val flatmapped: String +: CNil = v.flatMapC[SuperXY](_.toString).dedup
    flatmapped should be(Inl("X"))
  }

  "invariant flatMapping" in {
    type R = String :+: Byte
    def i2R(i: Int): R = {
      if (i > 0) {12.toByte.inject[R] }
      else {"blah".inject[R] }
    }

    val flatMapped: String +: Byte :+: Short = intVal.flatMapI[Int](i2R).dedup
    flatMapped should be(Inr(Inl(12)))

    val flatMapped1: String +: Byte :+: Short = intVal.mapI[Int](i2R).flatten.dedup
    flatMapped1 should be(Inr(Inl(12)))

    val flatMapped2: String +: Byte :+: Short = intVal.flatMapI[Int](v => i2R(v - 100)).dedup
    flatMapped2 should be(Inl("blah"))

    //this behavior is  possible because flatmap is implemented as map and then flatten
    //TODO: think of disabling this behavior in flatMap, it should be supported as part of 'mapI' case
    val flatMapped3: +:[String, +:[Short, CNil]] = intVal.flatMapI[Int](_.toString).dedup
    flatMapped3 should be(Inl("1"))
  }

  "flattening" in {

    val nested = Add(1).to[(Byte +: Int :+: Int) +: String +: Int :+: (Short :+: Int)]

    val flattened: Byte +: String +: Short :+: Int = nested.flatten.dedup
    flattened should be(Inr(Inr(Inr(Inl(1)))))

    val ft = flattenDedupType[(Byte +: Int +:(Int:+:String)) +: String +: Int +: (Short :+: Int) :+: Double]
    (1.inject[ft.Out]: Byte +: String +: Short +: Int :+: Double) should be(Inr(Inr(Inr(Inl(1)))))
  }



  "extend with single type" in {
    val extended: Boolean +: String +: Int :+: Short = intVal.extendWith[Boolean]
    extended should be(Inr(Inr(Inl(1))))
  }

  "case syntax" in {

    trait Err

    case object Err1 extends Err

    case object Err2 extends Err

    case object Err3 extends Err

    val process1: String = {
      Err1.inject[Err1.type +: Err2.type :+: Err3.type]
        ._case[Err1.type](_.toString)
        ._case[Err3.type](_.toString)
        ._case[Err2.type](_.toString)
    }

    process1 should be("Err1")

    val process2: String = {
      Err2.inject[Err1.type +: Err2.type :+: Err3.type]
        ._case[Err1.type](_.toString)
        ._caseAll[Err](_.toString)
    }

    process2 should be("Err2")

    val wrapExceptionOnly: Err2.type :+: Err3.type = {
      Err2.inject[Err1.type +: Err2.type :+: Err3.type]
        //Return type is stated explicitly because of the known inference problem
        ._case[Err1.type](_ => (throw new RuntimeException("")): Int)
        .suspend.left.get
    }

    wrapExceptionOnly should be(Inl(Err2))

    """val process3: String = {
      Inject(Err2).into[Err1.type +: Err2.type :+: Err3.type]
      ._case[Err1.type ](_.toString)
      ._case[Err2.type](_.toString)
    }""" shouldNot typeCheck

  }

  "caseAll, caseOthers, suspend" in {

    trait ServiceErr extends NoCauseError
    case class IOErr(msg: String) extends ServiceErr
    trait TimeOut extends ServiceErr

    type commonSuper = ValidationErr +: IOErr :+: TimeOut

    ValidationErr("err", null).inject[commonSuper]
      ._caseAll[GenError](_.toString) should be("ValidationErr(err,null)")


    val partiallyHandled: Either[ValidationErr, String] ={
      ValidationErr("err", null).inject[commonSuper]
        ._caseAll[ServiceErr](_.toString)
        .suspend
    }
    partiallyHandled should be(Left(ValidationErr("err", null)))

    val err: commonSuper = IOErr("boom").inject[commonSuper]

    val r2: String = err._case[IOErr](_.toString)._caseAll[GenError](_.toString)
    r2 should be ("IOErr(boom)")

    val r3: Either[ValidationErr :+: IOErr, String] = err._case[TimeOut](_.toString).suspend
    r3 should be (Left(Inr(Inl(IOErr("boom")))))

    val r4: Either[ValidationErr :+: TimeOut, String] = err._case[IOErr]("Recovered:" + _).suspend
    r4 should be (Right("Recovered:IOErr(boom)"))
  }
}
