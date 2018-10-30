package fintech.homework06

/*
Реализовать тайп класс Eq[A] и синтаксис '===', деривацию для Map Seq Option
Опционально - разработать === для комплексных чисел с возможностью указать точность
*/

trait Eq[-T] {
  def equiv(lft: T, rgt: Any): Boolean
}

object EqInstances {
  def atomInstance[T]: Eq[T] = new Eq[T] {
    def equiv(lft: T, rgt: Any): Boolean = lft == rgt
  }
  implicit val intInstance: Eq[Int] = atomInstance[Int]
  implicit val stringInstance: Eq[String] = atomInstance[String]
  implicit val noneInstance: Eq[None.type] = atomInstance[None.type]
}
object EqSyntax {
  implicit class EqOps[T: Eq](self: T) {
    def ====(other: Any): Boolean = implicitly[Eq[T]].equiv(self, other)
  }
}
}