package fintech.homework06
import org.scalatest.{FlatSpec, Matchers}

class AccEqSpec extends FlatSpec with Matchers {
  import fintech.homework06.AccEqSyntax._

  behavior of "Comparing with accuracy"

  it should "correctly work with Double types" in {
    (0.12345 ~~ "str")(5)   should be(false)
    (0.12345 ~~ 0.12399)(5) should be(false)
    (0.12345 ~~ 0.12399)(3) should be(true)
  }

  import fintech.homework02.ComplexNumber

  it should "correctly work with Complex Numbers" in {
    val first  = ComplexNumber(3.123456789, 5.123456789)
    val second = ComplexNumber(3.123456789, 5.123456799)
    val third  = ComplexNumber(3.129999999, 5.123456789)

    (first ~~ second)(8) should be(false)
    (first ~~ second)(5) should be(true)

    (first ~~ third)(4) should be(false)
    (first ~~ third)(1) should be(true)
  }
}
