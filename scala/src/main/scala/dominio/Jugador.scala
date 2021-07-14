package dominio

import util.Utils.Plata

// 5 - Modelar los 3 diferentes tipos de jugadores e inventar uno más a partir de lo que se haya modelado para crear
// jugadores nuevos a partir de criterios.

case class Jugador(montoInicial: Plata, condicion: CriterioEleccion)
  extends (List[JuegosSucesivos] => Option[(JuegosSucesivos, DistribucionJugadas)]) {

  def apply(combinacionesDeJuegos: List[JuegosSucesivos]): Option[(JuegosSucesivos, DistribucionJugadas)] = {
    combinacionesDeJuegos.maxByOption(juegosSucesivos => condicion(juegosSucesivos(montoInicial), montoInicial)) match {
      case Some(juegosSucesivo) => Some((juegosSucesivo, juegosSucesivo(montoInicial)))
      case None => None
    }
  }
}

sealed trait CriterioEleccion extends ((DistribucionJugadas, Plata) => Int) {
  def apply(distribucion: DistribucionJugadas, montoInicial: Plata): Int
}

case class Racional() extends CriterioEleccion {
  def apply(distribucion: DistribucionJugadas, montoInicial: Plata): Int =
    ( for (s <- distribucion.distribucion) yield s.suceso * s.probabilidad ).sum.toInt
}

case class Arriesgado() extends CriterioEleccion {
  def apply(distribucion: DistribucionJugadas, montoInicial: Plata): Int =
    distribucion.distribucion.maxBy(_.suceso).suceso
}

case class Cauto() extends CriterioEleccion {
  def apply(distribucion: DistribucionJugadas, montoInicial: Plata): Int =
    ( for (s <- distribucion.distribucion if s.suceso >= montoInicial) yield s.probabilidad ).sum.toInt
}

case class Inventado() extends CriterioEleccion {
  def apply(distribucion: DistribucionJugadas, montoInicial: Plata): Int = distribucion.sucesosPosibles.length
}
