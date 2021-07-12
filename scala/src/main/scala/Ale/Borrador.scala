package Ale

import scala.language.implicitConversions


/*def tieneEventoSeguro(): Boolean = distribucion.length == 1

def esEquiprobable(): Boolean = {
  import Ale.comparadorDoubles.Comparador
  val unaProbabilidad = distribucion.head.probabilidad
  distribucion.forall(_.probabilidad ~= unaProbabilidad)
}
}*/




/*// version sin fold, usando recursividad
  // sucesoPadre inicial --> SucesoConProbabilidad(Ganancia(montoInicial), 100.0)
  def apply2(montoInicial: Int, sucesoPadre: SucesoConProbabilidad): DistribucionProbabilidad = {
    apuestas.head match {
        case simple: ApuestaSimple =>
          if (listaSinCabeza().nonEmpty && montoInicial >= simple.montoApostado) {
            simple.distribucionDeGanancias().distribucion
              .map(_.sucesoGenerico.asInstanceOf[Ganancia].monto)
              .map(ganancia => JuegosSucesivos(listaSinCabeza()).apply2(montoInicial - simple.montoApostado + ganancia, sucesoPadre))
              .reduce((dist1, dist2) => dist1 ++ dist2)
          } else if (listaSinCabeza().isEmpty && montoInicial >= simple.montoApostado) {
            simple.distribucionDeGanancias() * (sucesoPadre, simple.montoApostado)
          } else if (siguiente().nonEmpty) {
            JuegosSucesivos(siguiente()).apply2(montoInicial,
              GeneradorDistribuciones().eventoSeguro(Ganancia(montoInicial)).distribucion.head) * sucesoPadre
          }  else {
            GeneradorDistribuciones().eventoSeguro(Ganancia(montoInicial)) * sucesoPadre
          }
        case combinada: ApuestaCombinada => DistribucionProbabilidad(List()) // TODO: REVISAR
      }
    }

  private def listaSinCabeza(): List[Apuesta] = apuestas match {
    case ::(_, next) => next
    case Nil => List()
  }

  private def siguiente(): List[Apuesta] = listaSinCabeza() match {
    case ::(_, next) => next
    case Nil => List()
  }*/


/*def ++(dist: DistribucionProbabilidad): DistribucionProbabilidad = {
    DistribucionProbabilidad(dist.copy().distribucion ++ copy().distribucion)
  }

  def *(suceso: SucesoConProbabilidad, montoApostado: Int): DistribucionProbabilidad = {
    DistribucionProbabilidad(
      copy().distribucion.map(
        su => SucesoConProbabilidad(
          Ganancia(
            suceso.sucesoGenerico.asInstanceOf[Ganancia].monto
              - montoApostado + su.sucesoGenerico.asInstanceOf[Ganancia].monto
          ), su.probabilidad * suceso.probabilidad
        )
      )
    )
  }

  def *(suceso: SucesoConProbabilidad): DistribucionProbabilidad = {
    DistribucionProbabilidad(
      copy().distribucion.map(
        su => SucesoConProbabilidad( su.sucesoGenerico, su.probabilidad * suceso.probabilidad )
      )
    )
  }*/

