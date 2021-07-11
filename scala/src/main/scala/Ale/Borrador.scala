package Ale

import scala.language.implicitConversions


/*case class GeneradorDistribuciones() {
  def eventoSeguro[T <: ResultadoDeJuego](suceso: T): DistribucionProbabilidad[T] = {
    DistribucionProbabilidad(List(SucesoConProbabilidad(suceso, 100.0)))
  }
  def distribucionEquiprobable[T <: ResultadoDeJuego](sucesos: List[T]): DistribucionProbabilidad[T] = {
    val cantidad: Int = sucesos.length
    DistribucionProbabilidad(sucesos.map(suceso => SucesoConProbabilidad(suceso, 100.0 / cantidad)))
  }
  def distribucionPonderada[T <: ResultadoDeJuego](sucesos: List[SucesoPonderado[T]]): DistribucionProbabilidad[T] = {
    val pesoTotal: Int = sucesos.map(_.pesoPonderado).sum
    DistribucionProbabilidad(sucesos.map(_.pasarASucesoProbable(pesoTotal)))
  }
}*/

/*case class GeneradorEventoSeguro[T <: ResultadoDeJuego](suceso: SucesoBasico[T]) {
  def apply(): DistribucionProbabilidad[T] = {
    DistribucionProbabilidad(List(SucesoConProbabilidad(suceso.resultadoDeJuego, 100.0)))
  }
}

case class GeneradorEquiprobable[T <: ResultadoDeJuego](sucesos: List[SucesoBasico[T]]) {
  def apply(): DistribucionProbabilidad[T] = {
    val cantidad: Int = sucesos.length
    DistribucionProbabilidad(sucesos.map(_.pasarASucesoProbable(cantidad)))
  }
}

case class GeneradorPonderado[T <: ResultadoDeJuego](sucesos: List[SucesoPonderado[T]]) {
  def apply(): DistribucionProbabilidad[T] = {
    val pesoTotal: Int = sucesos.map(_.pesoPonderado).sum
    DistribucionProbabilidad(sucesos.map(_.pasarASucesoProbable(pesoTotal)))
  }
}*/


/*case object SucesosCaraCruz {
  val sucesos: List[ResultadoCaraCruz] = List(Cara, Cruz)
}
case object SucesosRuleta {
  val sucesos: List[ResultadoRuleta] = (0 to 36).map(i => ResultadoRuleta(i)).toList
}



case class SucesoPonderado[T <: ResultadoDeJuego](resultadoDeJuego: T, pesoPonderado: Int) {
  def pasarASucesoProbable(pesoTotal: Int): SucesoConProbabilidad[T] = {
    SucesoConProbabilidad(resultadoDeJuego, (100.0 / pesoTotal) * pesoPonderado)
  }
}

case class SucesoConProbabilidad[T <: ResultadoDeJuego](resultadoDeJuego: T, probabilidad: Double)

case class DistribucionProbabilidad[T <: ResultadoDeJuego](distribucion: List[SucesoConProbabilidad[T]]) {
  def sucesosPosibles() : List[T] = distribucion.filter(_.probabilidad > 0.0).map(_.resultadoDeJuego)

  def probabilidadDe(suceso: T) :Double = {
    distribucion.find(su => su.resultadoDeJuego == suceso) match {
      case Some(resultado) => resultado.probabilidad
      case _ => 0.0
    }
  }*/





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




/*sealed trait Juego
sealed trait Jugada*/

/*case class CaraCruz(jugada: JugadaCaraCruz) extends Juego {
  def apply(montoApostado: Ganancia): ApuestaCaraCruz =
    if (jugada == ACara()) DuplicarSiSaleCara(montoApostado) else DuplicarSiSaleCruz(montoApostado)
}

sealed trait JugadaCaraCruz extends Jugada
case class ACara() extends JugadaCaraCruz
case class ACruz() extends JugadaCaraCruz*/

/*case class Ruleta(jugada: JugadaRuleta) extends Juego {
  def apply(montoApostado: Int): ApuestaRuleta = jugada match {
    case AlRojo() => JugarAlRojo(montoApostado)
    case AlNegro() => JugarAlNegro(montoApostado)
    case APar() => JugarAPar(montoApostado)
    case AImpar() => JugarAImpar(montoApostado)
    case AlNumero(numeroApostado) => JugarAlNumero(montoApostado, numeroApostado)
    case ADocena(docenaElegida) => JugarADocena(montoApostado, docenaElegida)
  }
}

sealed trait JugadaRuleta extends Jugada
case class AlRojo() extends JugadaRuleta
case class AlNegro() extends JugadaRuleta
case class APar() extends JugadaRuleta
case class AImpar() extends JugadaRuleta
case class AlNumero(numeroApostado: Int) extends JugadaRuleta
case class ADocena(docenaElegida: Docena) extends JugadaRuleta*/