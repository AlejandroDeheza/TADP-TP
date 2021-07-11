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

sealed trait CriterioEleccion extends (DistribucionGanancias => Double)

case class Racional() extends CriterioEleccion {
  def apply(distribucion: DistribucionGanancias): Double = {
    distribucion.distribucion
      .map(d => d.monto * d.probabilidad)
      .sum
  }
}

case class Arriesgado() extends CriterioEleccion {
  def apply(distribucion: DistribucionGanancias): Double = {
    distribucion.distribucion
      .maxBy(s => s.monto).monto
  }
}

case class Cauto(montoInicial: Plata) extends CriterioEleccion {
  def apply(distribucion: DistribucionGanancias): Double = {
    distribucion.distribucion
      .filter(s => s.monto >= montoInicial)
      .map(s => s.probabilidad)
      .sum
  }
}

case class Inventado() extends CriterioEleccion {
  def apply(distribucion: DistribucionGanancias): Double = {
    distribucion.sucesosPosibles().length
  }
}
