package fintech.homework02

// Написать класс описывающий комплексные числа.
// Реализовать проверку на равенство, умножение и сложение, toString.
// Реализовать оператор возведения в целую степень: "~".
// Реализовать тесты в ComplexNumberSpec

final case class ComplexNumber(real: Double, imaginary: Double) {
  def +(that: ComplexNumber): ComplexNumber =
    ComplexNumber(this.real + that.real, this.imaginary + that.imaginary)

  def *(that: ComplexNumber): ComplexNumber =
    ComplexNumber(this.real * that.real - this.imaginary * that.imaginary,
                  this.imaginary * that.real + this.real * that.imaginary)

  def ~(exponent: Int): ComplexNumber = {
    import scala.math._

    val thisPhi = atan2(imaginary, real)
    val thisR   = sqrt(real * real + imaginary * imaginary)

    val newPhi = thisPhi * exponent
    val newR   = pow(thisR, exponent)

    ComplexNumber(newR * cos(newPhi), newR * sin(newPhi))
  }

  override def equals(other: Any): Boolean = other match {
    case that: ComplexNumber => that.real == this.real && that.imaginary == this.imaginary
    case _                   => false
  }

  override def hashCode(): Int = (real, imaginary).hashCode()

  override def toString: String = {
    val imaginarySign = imaginary match {
      case d: Double if (d > 0) && (real != 0) => " + "
      case d: Double if (d < 0) && (real != 0) => " - "
      case d: Double if  d < 0                 => "-"
      case _                                   => ""
    }

    val realString =
      if (imaginary != 0 && real == 0)
        ""
      else if (real.floor == real) // чтобы избежать
        real.toInt.toString        // вывода чисел вида "2.0"
      else
        real.toString

    val imaginaryString =
      if (imaginary != 0) {
        if (imaginary.floor == imaginary)
          imaginary.toInt.abs.toString
        else
          imaginary.abs.toString
      } + "i"
      else
        ""

    realString + imaginarySign + imaginaryString
  }
}