package Ale

object Utils {
  type Plata = Int
  type ResultadoRuleta = Int

  def head[A](iterable: Iterable[A]): A = {
    iterable.head
  }

  def last[A](iterable: Iterable[A]): A = {
    iterable.last
  }
}
