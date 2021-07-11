package Ale

object comparadorDoubles {
  implicit val precision: Precision = Precision(0.0001)

  case class Precision(p:Double)

  implicit class Comparador(val d:Double) extends AnyVal {
    def ~=(d2:Double)(implicit p:Precision): Boolean = (d - d2).abs < p.p
  }
}

package object algo {
  implicit class ListExtensions[T](list: List[T]) {
    def maxByOption[B: Ordering](f: T => B): Option[T] = list match {
      case Nil => None
      case _ => Some(list.maxBy(f))
    }
  }
}
