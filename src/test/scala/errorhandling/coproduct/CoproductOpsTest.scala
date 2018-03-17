//package errorhandling.coproduct

//import org.scalatest.Matchers
//import org.scalatest.WordSpec
//
//class CoproductOpsTest extends WordSpec with Matchers {
//
//  val intVal: String +: Int :+: Short = Inject(1).to[String +: Int :+: Short]
//
//  trait SuperXY
//  case object X extends SuperXY
//  case object Y extends SuperXY
//
//  "polymap" in {
//
//    val oneToOneCoverage = {
//      PolyMap.builder
//        .flatMapI[String](identity)
//        .mapI[Int](_.toShort)
//        .mapI[SuperXY](_ => ())
//        .flatMapI[Byte](b => Inject(b.toString).to[String +: Cnil])
//        .done
//    }
//
//    type res = String +: Short +: X.type +: Unit :+: Boolean
//
//    val r1: res = oneToOneCoverage(Inject(1).to[Byte +: Int +: X.type +: SuperXY :+: Boolean])
//    r1 shouldBe CRight(CLeft(1))
//
//    val widerCoverage = {
//      PolyMap.builder
//        .flatMapI[Byte](b => Inject(b.toString).to[String +: Cnil])
//        .flatMapC[Exception](_.toString)
//        .mapI[Char](identity)
//        .mapI[SuperXY](_ => ())
//        .mapI[Int](_.toShort)
//        .flatMapI[String](_ + "!WARNING!")
//        .done
//    }
//    val r2: res = widerCoverage(Inject(1).to[Byte +: Int +: X.type +: SuperXY :+: Boolean])
//    r2 shouldBe r1
//
//    val r3 : String +: Short +: X.type :+: Boolean = {
//      widerCoverage(Inject(new RuntimeException(""): Exception ).to[Int +: Exception +: X.type :+: Boolean])
//    }
//    r3 shouldBe CLeft("java.lang.RuntimeException: !WARNING!")
//
//    val f = PolyMap.builder.mapI[Int](_ + 1).flatMapI[Byte](_ + 1).mapC[Exception](new RuntimeException("", _)).done
//    f(1) shouldBe(2)
//    f(new Exception("")).isInstanceOf[RuntimeException] shouldBe true
//    f("noMatch") shouldBe "noMatch"
//
//    val fmult = PolyMap.builder.flatMapI[Int](b=> Inject(b).to[Int:+:Char]).mapC[Exception](new RuntimeException("", _)).done
//    fmult(1) shouldBe CLeft(1)
//
//
//  }
//  "adding/injecting a value" in {
//
//    type C = String :+: Boolean
//
//    val i: Int +: String :+: Boolean = Add(1).to[C]
//    i should be(CLeft(1))
//
//    val i1: C = Inject("s").to[C]
//    i1 should be(CLeft("s"))
//
//    """Inject(1).to[C]""" shouldNot typeCheck
//    """Inject(Cnil).to[Int +: Cnil]""" shouldNot typeCheck
//  }
//
//  "extracting/removing values (invariant)" in {
//    val extracted: Either[String :+: Short, Int] = intVal.extract[Int]
//    extracted should be(Right(1))
//
//    val extracted1: Either[Int :+: Short, String] = intVal.extract[String]
//    extracted1 should be(Left(CLeft(1)))
//
//    intVal.extractAll[Any] should be(Right(1))
//
//    CRight[Int, Cnil](Cnil).extract[Int] should be(Left(Cnil))
//
//    val removed: Option[String :+: Short] = intVal.remove[Int]
//    removed shouldBe None
//
//    val removed1: Option[String :+: Int] = intVal.remove[Short]
//    removed1 shouldBe Some(CRight(CLeft(1)))
//
//    intVal.contains[Int] shouldBe(true)
//    intVal.contains[String] shouldBe(false)
//    """intVal.contains[Char]""" shouldNot typeCheck
//  }
//
//
//  "extracting values (covariant)" in {
//
//    type alltype = Y.type +: Int +: X.type :+: SuperXY
//
//    val intInjected = Inject(1).to[alltype]
//
//    val all: Either[Int +: Cnil, SuperXY] = intInjected.extractAll[SuperXY]
//    all should be(Left(CLeft(1)))
//
//    """intInjected.getAll[Byte]""" shouldNot typeCheck
//
//    val all1: Either[Int +: Cnil, SuperXY] = Inject(X).to[alltype].extractAll[SuperXY]
//    all1 should be(Right(X))
//  }
//
//  "getting the least upper bound value" in {
//    val x = Inject(X).to[X.type :+: Y.type]
//
//    val lubRes: SuperXY = x.lub
//    lubRes should be(X)
//  }
//
//  "mapping single value(covariant)" in {
//    val v: String +: X.type :+: Y.type = CRight(CLeft(X))
//    val mapped: Int :+: String = v.mapC[SuperXY](_ => 1)
//    mapped should be(CLeft(1))
//  }
//
//  "mapping single value(invariat)" in {
//    val mapped: String +: Boolean :+: Short = intVal.mapI[Int](_ == 1)
//    mapped should be(CRight(CLeft(true)))
//  }
//
//  "flatMapping(covariant)" in {
//    val v: String +: X.type :+: Y.type = CRight(CLeft(X))
//    val flatmapped: String +: Cnil.type = v.flatMapC[SuperXY](_.toString)
//    flatmapped should be(CLeft("X"))
//  }
//
//  "flatMapping(invariant)" in {
//    type R = String :+: Byte
//    def i2R(i: Int): R = {
//      if (i > 0) {Inject(12.toByte).to[R] }
//      else {Inject("blah").to[R] }
//    }
//
//    val flatMapped: String +: Byte :+: Short = intVal.flatMapI[Int](i2R)
//    flatMapped should be(CRight(CLeft(12)))
//
//    val flatMapped1: String +: Byte :+: Short = intVal.mapI[Int](i2R).flatten
//    flatMapped1 should be(CRight(CLeft(12)))
//
//    val flatMapped2: String +: Byte :+: Short = intVal.flatMapI[Int](v => i2R(v - 100))
//    flatMapped2 should be(CLeft("blah"))
//
//    //this behavior is  possible because flatmap is implemented as map and then flatten
//    //TODO: think of disabling this behavior in flatMap, it should be supported as part of 'mapI' case
//    val flatMapped3: +:[String, +:[Short, Cnil.type]] = intVal.flatMapI[Int](_.toString)
//    flatMapped3 should be(CLeft("1"))
//  }
//
//  //TODO: it should flatten only one level
//  "flattening (any levels of nesting)" in {
//
//    val nested = Add(1).to[(Byte :+: Int :+: Int) +: String +: Int :+: (Short :+: Int)]
//
//    val flattened: Byte +: String +: Short :+: Int = nested.flatten
//    flattened should be(CRight(CRight(CRight(CLeft(1)))))
//
//    val ft = Flatten[(Byte :+: Int) +: String +: Int :+: (Short :+: Int) :+: Double]
//    Inject(1).to[ft.Out].isInstanceOf[Byte +: String +: Short +: Int :+: Double] should be(true)
//  }
//
//  "diff with other coproduct" in {
//    val diffed: Option[Short +: Cnil] = intVal.diff[Int :+: String]
//    diffed should be(None)
//
//    val diffed1: Option[Int +: Cnil] = intVal.diff[Short :+: String]
//    diffed1 should be(Some(CLeft(1)))
//  }
//
//  "aligning to shape" in {
//    val aligned: Int +: Double +: String :+: Short = intVal.align[Int +: Double +: String :+: Short]
//    aligned should be(CLeft(1))
//  }
//
//  "merge syntax" in {
//    val appended: String +: Int +: Short +: Byte :+: Char = intVal.append[Byte :+: Char]
//    appended should be(CRight(CLeft(1)))
//
//    //deduplication is performed
//    val appended1: Int +: Short +: String :+: Char = intVal.append[String :+: Char]
//    appended1 should be(CLeft(1))
//
//    val prepended: Byte +: String +: Int :+: Short = intVal.prepend[Byte :+: Int]
//    prepended should be(CRight(CRight(CLeft(1))))
//  }
//
//  "extend with single type" in {
//    val extended: Boolean +: String +: Int :+: Short = intVal.extendWith[Boolean]
//    extended should be(CRight(CRight(CLeft(1))))
//  }
//
//  "case syntax" in {
//
//    trait Err
//
//    case object Err1 extends Err
//
//    case object Err2 extends Err
//
//    case object Err3 extends Err
//
//    val process1: String = {
//      Inject(Err1).to[Err1.type +: Err2.type :+: Err3.type]
//        ._case[Err1.type].apply(_.toString)
//        ._case[Err3.type](_.toString)
//        ._case[Err2.type](_.toString)
//    }
//
//    process1 should be("Err1")
//
//    val process2: String = {
//      Inject(Err2).to[Err1.type +: Err2.type :+: Err3.type]
//        ._case[Err1.type].apply(_.toString)
//        ._caseAll[Err](_.toString)
//    }
//
//    process2 should be("Err2")
//
//    val wrapExceptionOnly: Err2.type :+: Err3.type = {
//      Inject(Err2).to[Err1.type +: Err2.type :+: Err3.type]
//        //Return type is stated explicitly because of the known inference problem
//        ._case[Err1.type].apply(_ => (throw new RuntimeException("")): Int)
//        .suspend.left.get
//    }
//
//    wrapExceptionOnly should be(CLeft(Err2))
//
//    """val process3: String = {
//      Inject(Err2).into[Err1.type +: Err2.type :+: Err3.type]
//      ._case[Err1.type ](_.toString)
//      ._case[Err2.type](_.toString)
//    }""" shouldNot typeCheck
//
//  }
//}
