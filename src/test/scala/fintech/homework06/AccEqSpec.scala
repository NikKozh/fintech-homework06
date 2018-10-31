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
}
