package Ale

import Ale.Utils.{Plata, ResultadoDeJuego, head, last}

sealed trait EstadoApuesta
case class Gano() extends EstadoApuesta
case class Perdio() extends EstadoApuesta
case class NoJugo() extends EstadoApuesta

case class SucesoConEstados(suceso: SucesoConProbabilidad[Plata], historialDeEstados: List[EstadoApuesta])



// Tomo al juego y la apuesta como el mismo objeto ---> Juego
sealed trait Juego {
  def ampliarDistribucion(distInicial: DistribucionProbabilidad[Plata]): DistribucionProbabilidad[Plata]
}

trait JuegoSimple[T] extends Juego {
  val montoApostado: Plata
  def apply(resultado: T): Plata
  def perder(): Plata = 0
  def ganarDoble(): Plata = montoApostado * 2
  def distribucionGanancias(): DistribucionProbabilidad[Plata]

  override def ampliarDistribucion(distInicial: DistribucionProbabilidad[Plata]): DistribucionProbabilidad[Plata] = {

    val sucesosNormales =     for (s <- distInicial.distribucion if s.suceso >= montoApostado) yield s
    val sucesosConPocaPlata = for (s <- distInicial.distribucion if s.suceso < montoApostado)  yield s

    val sucesosNuevos =  for (s <- sucesosNormales) yield generarSuceso(head, s)
    val sucesosNuevos2 = for (s <- sucesosNormales) yield generarSuceso(last, s)
    //val sucesosNuevos3 = for (s <- sucesosConPocaPlata) yield generarSuceso(_, s)

    DistribucionProbabilidad[Plata](sucesosNuevos ++ sucesosNuevos2 ++ sucesosConPocaPlata)
  }

  private def generarSuceso(f: Iterable[SucesoConProbabilidad[Plata]] => SucesoConProbabilidad[Plata],
                            s: SucesoConProbabilidad[Plata]): SucesoConProbabilidad[Plata] = {
    val montoInicial = s.suceso
    val ganancia = f(distribucionGanancias().copy().distribucion).suceso
    val multiplicacionProbabilidades = s.probabilidad * f(distribucionGanancias().copy().distribucion).probabilidad

    SucesoConProbabilidad( montoInicial - montoApostado + ganancia, multiplicacionProbabilidades )
  }
}


// 2 - Permitir crear apuestas compuestas para los juegos cuyos resultados se modelaron en el punto anterior.
// JUEGO COMPUESTO <<------------------------------
case class JuegoCompuesto[T : ResultadoDeJuego](juegosSimples: List[JuegoSimple[T]]) extends (T => Plata) with Juego {

  def apply(resultado: T): Plata = ( for (j <- juegosSimples) yield j(resultado) ).sum

  override def ampliarDistribucion(distInicial: DistribucionProbabilidad[Plata]): DistribucionProbabilidad[Plata] = {
    juegosSimples.foldLeft(distInicial.copy()){ (distrib, juegoSimple) => juegoSimple.ampliarDistribucion(distrib) }
  }
}


// 4 - Permitir que un jugador juegue sucesivamente varios juegos
// JUEGOS SUCESIVOS <<---------------------------------------
case class JuegosSucesivos(juegos: List[Juego]) {

  def apply(montoInicial: Plata): DistribucionProbabilidad[Plata] = {
    //val semilla = List(SucesoConEstados(SucesoConProbabilidad(montoInicial, 1.0), List()))
    juegos.foldLeft(DistribucionProbabilidad[Plata](List(SucesoConProbabilidad(montoInicial, 1.0)))) {
      (distribucion, juego) => juego.ampliarDistribucion(distribucion)
    }
  }
  // TODO: TESTEA BIEN ESTO, ES EL CORE DEL TP
  // TODO: fijate si es facil agregar lo que decia sobre el historial de cierto suceso
}
