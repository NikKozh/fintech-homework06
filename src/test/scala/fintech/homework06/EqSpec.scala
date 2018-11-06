package fintech.homework06
import org.scalatest.{FlatSpec, Matchers}

class EqSpec extends FlatSpec with Matchers {
  import fintech.homework06.EqSyntax._

  behavior of "==== method"

  it should "correctly compare Int types" in {
    1 ==== 2 should be(false)
    2 ==== 2 should be(true)
  }

  it should "correctly compare String types" in {
    "str" ==== "str0" should be(false)
    "str" ==== "str"  should be(true)
  }

  it should "correctly compare any Seq descendants in collection hierarchy" in {
    Seq(1, 2, 3) ==== Seq(1, 3, 2) should be(false)
    Seq(1, 2, 3) ==== Seq(1, 2, 3) should be(true)

    Seq(4, 5, 6) ==== Seq(4, 6, 5) should be(false)
    Seq(4, 5, 6) ==== Seq(4, 5, 6) should be(true)
  }

  it should "correctly compare Option types" in {
    val o1: Option[Int] = Some(1)
    val o2: Option[Int] = Some(2)

    o1 ==== o2 should be(false)
    o2 ==== o2 should be(true)
//    Some(1) ==== None    should be(false)
//    None    ==== Some(2) should be(false)
    None    ==== None    should be(true)
  }

  it should "correctly compare Map types" in {
    Map(1 -> "1", 2 -> "2") ==== Map(1 -> "1", 2 -> "4") should be(false)
    Map(1 -> "1", 3 -> "2") ==== Map(1 -> "1", 2 -> "2") should be(false)
    Map(1 -> "1", 2 -> "2") ==== Map(1 -> "1", 2 -> "2") should be(true)
  }

  it should "correctly compare complex nested types" in {
    val o1: Option[Seq[Int]] = Some(Seq(2, 3))
    val o2: Option[Seq[Int]] = Some(Seq(2, 4))

    o1 ==== o2 should be(false)
    o1 ==== o1 should be(true)

    Map(1 -> Seq(Some(1), None), 2 -> Seq(Some(2), None)) ====
      Map(1 -> Seq(Some(1), None), 2 -> Seq(None, Some(2))) should be(false)

    Map(1 -> Seq(Some(1), None), 2 -> Seq(Some(2), None)) ====
      Map(1 -> Seq(Some(1), None), 2 -> Seq(Some(2), None)) should be(true)
  }
}
