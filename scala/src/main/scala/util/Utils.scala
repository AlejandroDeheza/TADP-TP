package util

import dominio.ResultadoCaraCruz

object Utils {
  type Plata = Int
  type ResultadoRuleta = Int

  sealed trait ResultadoDeJuego[T]
  object ResultadoDeJuego {
    implicit val resultadoRuleta: ResultadoDeJuego[ResultadoRuleta] = new ResultadoDeJuego[ResultadoRuleta] {}
    implicit val resultadoCaraCruz: ResultadoDeJuego[ResultadoCaraCruz] = new ResultadoDeJuego[ResultadoCaraCruz] {}
  }

}
