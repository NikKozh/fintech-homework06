package fintech.homework06

/*
Реализовать тайп класс Eq[A] и синтаксис '===', деривацию для Map Seq Option
Опционально - разработать === для комплексных чисел с возможностью указать точность
*/

trait Eq[-T] {
  def equiv(lft: T, rgt: T): Boolean
}

trait EqInstances {
  def atomInstance[T]: Eq[T] = new Eq[T] {
    def equiv(lft: T, rgt: T): Boolean = lft == rgt
  }
  implicit val intInstance:    Eq[Int]       = atomInstance[Int]
  implicit val stringInstance: Eq[String]    = atomInstance[String]
  implicit val noneInstance:   Eq[None.type] = atomInstance[None.type]

  implicit def listInstance[T: Eq]: Eq[Seq[T]] = new Eq[Seq[T]] {
    def equiv(lft: Seq[T], rgt: Seq[T]): Boolean = {
      val eq = implicitly[Eq[T]]
      lft.size == rgt.size && lft.zip(rgt).forall(pair => eq.equiv(pair._1, pair._2))
    }
  }

  implicit def optionInstance[T: Eq]: Eq[Option[T]] = new Eq[Option[T]] {
    def equiv(lft: Option[T], rgt: Option[T]): Boolean = (lft, rgt) match {
      case (None, None)         => true
      case (Some(x1), Some(x2)) => implicitly[Eq[T]].equiv(x1, x2)
      case _                    => false
    }
  }

  implicit def mapInstance[K: Eq, T: Eq]: Eq[Map[K, T]] = new Eq[Map[K, T]] {
    def equiv(lft: Map[K, T], rgt: Map[K, T]): Boolean = {
      val keysEq   = implicitly[Eq[K]]
      val valuesEq = implicitly[Eq[T]]

      lft.size == rgt.size && lft.keySet.zip(rgt.keySet).forall(keysPair => keysEq.equiv(keysPair._1, keysPair._2)) &&
      lft.values.zip(rgt.values).forall(valuesPair => valuesEq.equiv(valuesPair._1, valuesPair._2))
    }
  }
}

object EqSyntax {
  implicit class EqOps[T: Eq](self: T) {
    def ====(other: T): Boolean = implicitly[Eq[T]].equiv(self, other)
  }
}

object Eq extends EqInstances