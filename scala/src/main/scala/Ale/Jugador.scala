package Ale

// 5 - Modelar los 3 diferentes tipos de jugadores e inventar uno más a partir de lo que se haya modelado para crear
// jugadores nuevos a partir de criterios.

/*object tipos {
  type CriterioEleccionJuegos[O : Ordering] = DistribucionProbabilidad => O
}*/

case class Jugador[O : Ordering](montoInicial: Int, condicion: CriterioEleccion)
  extends (List[JuegosSucesivos] => JuegosSucesivos) {
   def apply(combinacionesDeJuegos: List[JuegosSucesivos]): JuegosSucesivos = {
    combinacionesDeJuegos.maxByOption(juegosSucesivos => condicion(juegosSucesivos(montoInicial))).get
  }
}

sealed trait CriterioEleccion {
  def apply(distribucion: DistribucionProbabilidad): Double
}
case class Racional() extends CriterioEleccion {
  def apply(distribucion: DistribucionProbabilidad): Double = {
    distribucion.distribucion
      .map(d => d.sucesoGenerico.asInstanceOf[Ganancia].monto * d.probabilidad)
      .sum
  }
}

case class Arriesgado() extends CriterioEleccion {
  def apply(distribucion: DistribucionProbabilidad): Double = {
    distribucion.distribucion
      .maxBy(s => s.sucesoGenerico.asInstanceOf[Ganancia].monto).sucesoGenerico.asInstanceOf[Ganancia].monto
  }
}

case class Cauto(montoInicial: Int) extends CriterioEleccion {
  def apply(distribucion: DistribucionProbabilidad): Double = {
    distribucion.distribucion
      .filter(s => s.sucesoGenerico.asInstanceOf[Ganancia].monto >= montoInicial)
      .map(s => s.probabilidad)
      .product
  }
}

case class Inventado() extends CriterioEleccion {
  def apply(distribucion: DistribucionProbabilidad): Double = {
    distribucion.sucesosPosibles().length
  }
}
