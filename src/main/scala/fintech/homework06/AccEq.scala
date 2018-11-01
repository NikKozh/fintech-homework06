package fintech.homework06

trait AccEq[T] {
  def equivAcc(lft: T, rgt: T, precise: Int): Boolean
}

trait AccEqInstances {
  implicit val doubleInstance: AccEq[Double] = new AccEq[Double] {
    def equivAcc(lft: Double, rgt: Double, precise: Int): Boolean = {
      import math._
      val epsilon = 1.0 / pow(10, abs(precise))
      abs(lft - rgt) < epsilon
    }
  }

  import fintech.homework02.ComplexNumber
  import AccEqSyntax._

  implicit val complexNumInstance: AccEq[ComplexNumber] = new AccEq[ComplexNumber] {
    def equivAcc(lft: ComplexNumber, rgt: ComplexNumber, precise: Int): Boolean =
      (lft.real ~~ rgt.real)(precise) && (lft.imaginary ~~ rgt.imaginary)(precise)
  }
}

object AccEqSyntax {
  implicit class EqOps[T: AccEq](self: T) {
    def ~~(other: T)(precise: Int): Boolean = implicitly[AccEq[T]].equivAcc(self, other, precise)
  }
}

object AccEq extends AccEqInstances