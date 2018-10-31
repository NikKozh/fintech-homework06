package fintech.homework06

/*
Реализовать тайп класс Eq[A] и синтаксис '===', деривацию для Map Seq Option
Опционально - разработать === для комплексных чисел с возможностью указать точность
*/

trait Eq[-T] {
  /*
  * Мне кажется, что тип правого операнда должен быть Any, т.к. в этом случае мы можем
  * сравнить конкретный тип с чем угодно без ошибок компиляции (как это сделано для методов
  * == и isEqual в стандартной библиотеке), при этом pattern-matching всё ещё позволит нам
  * проводить корректное сравнение, если правый операнд такого же типа.
  * */
  def equiv(lft: T, rgt: Any): Boolean
}

trait EqInstances {
  def atomInstance[T]: Eq[T] = new Eq[T] {
    def equiv(lft: T, rgt: Any): Boolean = lft == rgt
  }
  implicit val intInstance:    Eq[Int]       = atomInstance[Int]
  implicit val stringInstance: Eq[String]    = atomInstance[String]
  implicit val noneInstance:   Eq[None.type] = atomInstance[None.type]

  implicit def listInstance[T: Eq]: Eq[Seq[T]] = new Eq[Seq[T]] {
    def equiv(lft: Seq[T], rgt: Any): Boolean = rgt match {
      case x: Seq[_] =>
        val eq = implicitly[Eq[T]]
        lft.size == x.size && lft.zip(x).forall(pair => eq.equiv(pair._1, pair._2))
      case _ =>
        false
    }
  }

  implicit def optionInstance[T: Eq]: Eq[Option[T]] = new Eq[Option[T]] {
    def equiv(lft: Option[T], rgt: Any): Boolean = (lft, rgt) match {
      case (None, None)         => true
      case (Some(x1), Some(x2)) => implicitly[Eq[T]].equiv(x1, x2)
      case _                    => false
    }
  }

  implicit def mapInstance[K: Eq, T: Eq]: Eq[Map[K, T]] = new Eq[Map[K, T]] {
    def equiv(lft: Map[K, T], rgt: Any): Boolean = rgt match {
      case x: Map[_, _] =>
        val keysEq = implicitly[Eq[K]]
        val valuesEq = implicitly[Eq[T]]

        // В таких случаях лучше ввести две-три лишние переменные и разбить выражение для лучшей читаемости
        // или лучше оставить вот так для краткости кода?
        lft.size == x.size && lft.keySet.zip(x.keySet).forall(keysPair => keysEq.equiv(keysPair._1, keysPair._2)) &&
        lft.values.zip(x.values).forall(valuesPair => valuesEq.equiv(valuesPair._1, valuesPair._2))
      case _ =>
        false
    }
  }
}

object EqSyntax {
  implicit class EqOps[T: Eq](self: T) {
    def ====(other: Any): Boolean = implicitly[Eq[T]].equiv(self, other)
  }
}

object Eq extends EqInstances