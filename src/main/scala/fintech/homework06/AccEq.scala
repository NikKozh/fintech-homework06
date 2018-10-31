package fintech.homework06

trait AccEq[T] {
  def equivAcc(lft: T, rgt: Any, precise: Int): Boolean
}

trait AccEqInstances {
  implicit val doubleInstance: AccEq[Double] = new AccEq[Double] {
    def equivAcc(lft: Double, rgt: Any, precise: Int): Boolean = rgt match {
      case x: Double =>
        import math._
        val epsilon = 1.0 / pow(10, abs(precise))
        abs(lft - x) < epsilon
      case _ =>
        false
    }
  }
}

object AccEqSyntax {
  implicit class EqOps[T: AccEq](self: T) {
    def ~~(other: Any)(precise: Int): Boolean = implicitly[AccEq[T]].equivAcc(self, other, precise)
  }
}

object AccEq extends AccEqInstances