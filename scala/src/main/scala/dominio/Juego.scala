package dominio

import util.Utils.{Plata, ResultadoDeJuego}

sealed trait EstadoApuesta
case class Gano(apuesta: JuegoSimple[_])   extends EstadoApuesta
case class Perdio(apuesta: JuegoSimple[_]) extends EstadoApuesta
case class NoJugo(apuesta: JuegoSimple[_]) extends EstadoApuesta



// Tomo al juego y la apuesta como el mismo objeto ---> Juego
sealed trait Juego {
  def ampliarDistribucion(distInicial: DistribucionJugadas): DistribucionJugadas
}

trait JuegoSimple[T] extends Juego {
  val montoApostado: Plata
  val distribucionGanancias: DistribucionProbabilidad[Plata]
  lazy val pierde: Plata = 0
  lazy val ganaDoble: Plata = montoApostado * 2

  def apply(resultado: T): Plata

  override def ampliarDistribucion(distInicial: DistribucionJugadas): DistribucionJugadas = {
    val seJugaron = for (s <- distInicial.distribucion if s.suceso >= montoApostado) yield s
    val sucesosNuevos =  for (s <- seJugaron) yield generarSuceso(0, s)
    val sucesosNuevos2 = for (s <- seJugaron) yield generarSuceso(1, s)

    val noSeJugaron =
      for (s <- distInicial.distribucion if s.suceso < montoApostado)
        yield s.copy(historialDeEstados = s.historialDeEstados ++ List(NoJugo(this)))

    DistribucionJugadas(sucesosNuevos ++ sucesosNuevos2 ++ noSeJugaron)
  }

  private def generarSuceso(indice: Int, s: SucesoConEstados): SucesoConEstados = {
    val montoInicial = s.suceso
    val ganancia = distribucionGanancias.distribucion(indice).suceso
    val nuevaProbabilidad = s.probabilidad * distribucionGanancias.distribucion(indice).probabilidad
    val estado = if (ganancia == 0) Perdio(this) else Gano(this)

    SucesoConEstados(
      montoInicial - montoApostado + ganancia, nuevaProbabilidad, s.historialDeEstados ++ List(estado)
    )
  }
}


// 2 - Permitir crear apuestas compuestas para los juegos cuyos resultados se modelaron en el punto anterior.
// JUEGO COMPUESTO <<------------------------------
case class JuegoCompuesto[T : ResultadoDeJuego](juegosSimples: List[JuegoSimple[T]]) extends (T => Plata) with Juego {

  def apply(resultado: T): Plata = ( for (j <- juegosSimples) yield j(resultado) ).sum

  override def ampliarDistribucion(distInicial: DistribucionJugadas): DistribucionJugadas = {
    juegosSimples.foldLeft(distInicial){ (distrib, juegoSimple) => juegoSimple.ampliarDistribucion(distrib) }
  }
}


// 4 - Permitir que un jugador juegue sucesivamente varios juegos
// JUEGOS SUCESIVOS <<---------------------------------------
case class JuegosSucesivos(juegos: List[Juego]) {

  def apply(montoInicial: Plata): DistribucionJugadas = {
    val semilla = DistribucionJugadas(List(SucesoConEstados(montoInicial, 1.0, List())))
    juegos.foldLeft(semilla) { (distribucion, juego) => juego.ampliarDistribucion(distribucion) }
  } // TODO: TESTEAR BIEN ESTO
}
