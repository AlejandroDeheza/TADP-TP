package Ale

import Ale.Utils.Plata

// 5 - Modelar los 3 diferentes tipos de jugadores e inventar uno más a partir de lo que se haya modelado para crear
// jugadores nuevos a partir de criterios.

case class Jugador(montoInicial: Plata, condicion: CriterioEleccion)
  extends (List[JuegosSucesivos] => JuegosSucesivos) {
  def apply(combinacionesDeJuegos: List[JuegosSucesivos]): JuegosSucesivos = {
    combinacionesDeJuegos.maxByOption(juegosSucesivos => condicion(juegosSucesivos(montoInicial))).get
  }
}

sealed trait CriterioEleccion extends (DistribucionProbabilidad[Plata] => Double)

case class Racional() extends CriterioEleccion {
  def apply(distribucion: DistribucionProbabilidad[Plata]): Double = {
    distribucion.distribucion
      .map(d => d.suceso * d.probabilidad)
      .sum
  }
}

case class Arriesgado() extends CriterioEleccion {
  def apply(distribucion: DistribucionProbabilidad[Plata]): Double = {
    distribucion.distribucion
      .maxBy(s => s.suceso).suceso
  }
}

case class Cauto(montoInicial: Plata) extends CriterioEleccion {
  def apply(distribucion: DistribucionProbabilidad[Plata]): Double = {
    distribucion.distribucion
      .filter(s => s.suceso >= montoInicial)
      .map(s => s.probabilidad)
      .sum
  }
}

case class Inventado() extends CriterioEleccion {
  def apply(distribucion: DistribucionProbabilidad[Plata]): Double = {
    distribucion.sucesosPosibles().length
  }
}
