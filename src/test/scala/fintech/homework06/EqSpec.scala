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
    List(1, 2, 3) ==== List(1, 3, 2) should be(false)
    List(1, 2, 3) ==== List(1, 2, 3) should be(true)

    Vector(4, 5, 6) ==== Vector(4, 6, 5) should be(false)
    Vector(4, 5, 6) ==== Vector(4, 5, 6) should be(true)
  }

  it should "correctly compare Option types" in {
    Some(1) ==== Some(2) should be(false)
    Some(2) ==== Some(2) should be(true)
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
    Some(List(2, 3)) ==== Some(List(2, 4)) should be(false)
    Some(List(2, 3)) ==== Some(List(2, 3)) should be(true)

    Map(1 -> List(Some(1), None), 2 -> List(Some(2), None)) ====
      Map(1 -> List(Some(1), None), 2 -> List(None, Some(2))) should be(false)

    Map(1 -> List(Some(1), None), 2 -> List(Some(2), None)) ====
      Map(1 -> List(Some(1), None), 2 -> List(Some(2), None)) should be(true)
  }
}
