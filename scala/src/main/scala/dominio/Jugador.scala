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

sealed trait CriterioEleccion extends ((DistribucionJugadas, Plata) => Double) {
  def apply(distribucion: DistribucionJugadas, montoInicial: Plata): Double
}

case class Racional() extends CriterioEleccion {
  def apply(distribucion: DistribucionJugadas, montoInicial: Plata): Double = {
    ( for (s <- distribucion.distribucion) yield s.suceso * s.probabilidad ).sum
  }
}

case class Arriesgado() extends CriterioEleccion {
  def apply(distribucion: DistribucionJugadas, montoInicial: Plata): Double = {
    distribucion.distribucion
      .maxBy(_.suceso).suceso
  }
}

case class Cauto() extends CriterioEleccion {
  def apply(distribucion: DistribucionJugadas, montoInicial: Plata): Double = {
    ( for (s <- distribucion.distribucion if s.suceso >= montoInicial) yield s.probabilidad ).sum
  }
}

case class Inventado() extends CriterioEleccion {
  def apply(distribucion: DistribucionJugadas, montoInicial: Plata): Double = distribucion.sucesosPosibles.length
}
