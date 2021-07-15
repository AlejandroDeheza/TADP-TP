package dominio

import util.Utils.Plata

// 5 - Modelar los 3 diferentes tipos de jugadores e inventar uno más a partir de lo que se haya modelado para crear
// jugadores nuevos a partir de criterios.

case class Jugador(montoInicial: Plata, condicion: CriterioEleccion)
  extends (List[ApuestasSucesivas] => Option[(ApuestasSucesivas, DistribucionApuestas)]) {

  def apply(apuestasSucesivas: List[ApuestasSucesivas]): Option[(ApuestasSucesivas, DistribucionApuestas)] = {
    apuestasSucesivas.maxByOption(unaApuestaSucesiva => condicion(unaApuestaSucesiva(montoInicial)))
      .map(unaApuestaSucesiva => (unaApuestaSucesiva, unaApuestaSucesiva(montoInicial)))
  }// TODO: creo que puedo cambiar el maxByOption por un maxBy
  // TODO: creo que con solo devolver la distribucion resultante es suficiente tambien
}

sealed trait CriterioEleccion extends (DistribucionApuestas => Int)

case object Racional extends CriterioEleccion {
  def apply(distribucion: DistribucionApuestas): Int =
    ( for (s <- distribucion.sucesos) yield s.valor * s.probabilidad ).sum.toInt
}

case object Arriesgado extends CriterioEleccion {
  def apply(distribucion: DistribucionApuestas): Int =
    distribucion.sucesos.maxBy(_.valor).valor
}

case object Cauto extends CriterioEleccion {
  def apply(distribucion: DistribucionApuestas): Int =
    ( for (s <- distribucion.sucesos if s.valor >= distribucion.montoInicial) yield s.probabilidad ).sum.toInt
}

case object Inventado extends CriterioEleccion {
  def apply(distribucion: DistribucionApuestas): Int = distribucion.sucesosPosibles.length
}

case class CriterioCustom(criterio: DistribucionApuestas => Int) extends CriterioEleccion {
  override def apply(distribucion: DistribucionApuestas): Plata =
    criterio(distribucion)
}
