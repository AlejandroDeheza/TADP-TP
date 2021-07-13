package Ale

import Ale.Utils.{Plata, ResultadoDeJuego}

sealed trait EstadoApuesta
case class Gano() extends EstadoApuesta
case class Perdio() extends EstadoApuesta
case class NoJugo() extends EstadoApuesta



// Tomo al juego y la apuesta como el mismo objeto ---> Juego
sealed trait Juego {
  def ampliarDistribucion(distInicial: DistribucionJugadas): DistribucionJugadas
}

trait JuegoSimple[T] extends Juego {
  val montoApostado: Plata
  def apply(resultado: T): Plata
  def perder(): Plata = 0
  def ganarDoble(): Plata = montoApostado * 2
  def distribucionGanancias(): DistribucionProbabilidad[Plata]

  override def ampliarDistribucion(distInicial: DistribucionJugadas): DistribucionJugadas = {
    val seJugaron = for (s <- distInicial.distribucion if s.suceso >= montoApostado) yield s
    val sucesosNuevos =  for (s <- seJugaron) yield generarSuceso(0, s)
    val sucesosNuevos2 = for (s <- seJugaron) yield generarSuceso(1, s)

    val noSeJugaron =
      for (s <- distInicial.distribucion if s.suceso < montoApostado)
        yield SucesoConEstados( s.suceso, s.probabilidad, s.copy().historialDeEstados ++ List(NoJugo()) )

    DistribucionJugadas(sucesosNuevos ++ sucesosNuevos2 ++ noSeJugaron)
  }

  private def generarSuceso(indice: Int, s: SucesoConEstados): SucesoConEstados = {
    val montoInicial = s.suceso
    val ganancia = distribucionGanancias().copy().distribucion(indice).suceso
    val nuevaProbabilidad = s.probabilidad * distribucionGanancias().copy().distribucion(indice).probabilidad
    val estado = if (ganancia == 0) Perdio() else Gano()

    SucesoConEstados(
      montoInicial - montoApostado + ganancia, nuevaProbabilidad, s.copy().historialDeEstados ++ List(estado)
    )
  }
}


// 2 - Permitir crear apuestas compuestas para los juegos cuyos resultados se modelaron en el punto anterior.
// JUEGO COMPUESTO <<------------------------------
case class JuegoCompuesto[T : ResultadoDeJuego](juegosSimples: List[JuegoSimple[T]]) extends (T => Plata) with Juego {

  def apply(resultado: T): Plata = ( for (j <- juegosSimples) yield j(resultado) ).sum

  override def ampliarDistribucion(distInicial: DistribucionJugadas): DistribucionJugadas = {
    juegosSimples.foldLeft(distInicial.copy()){ (distrib, juegoSimple) => juegoSimple.ampliarDistribucion(distrib) }
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
