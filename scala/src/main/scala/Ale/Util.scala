package Ale

object Utils {
  type Plata = Int
  type ResultadoRuleta = Int

  sealed trait ResultadoDeJuego[T]
  object ResultadoDeJuego {
    implicit val resultadoRuleta: ResultadoDeJuego[ResultadoRuleta] = new ResultadoDeJuego[ResultadoRuleta] {}
    implicit val resultadoCaraCruz: ResultadoDeJuego[ResultadoCaraCruz] = new ResultadoDeJuego[ResultadoCaraCruz] {}
  }

  def head[A](iterable: Iterable[A]): A = {
    iterable.head
  }

  def last[A](iterable: Iterable[A]): A = {
    iterable.last
  }
}

