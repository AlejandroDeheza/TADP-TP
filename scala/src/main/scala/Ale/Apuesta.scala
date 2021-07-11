package Ale

import Ale.Utils.Plata

sealed trait SucesoGenerico
//trait ResultadoDeJuego extends SucesoGenerico
case class PlataWrapper(monto: Plata) extends SucesoGenerico  // TODO: REVISAR

sealed trait Apuesta
// Tomo a la apuesta y la jugada como el mismo objeto ---> Apuesta

trait ApuestaSimple[T] extends Apuesta {
  val montoApostado: Plata
  def apply(resultado: T): Plata
  def perder(): Plata = 0
  def ganarDoble(): Plata = montoApostado * 2
  def distribucionGanancias(): DistribucionGanancias
}


// 2 - Permitir crear apuestas compuestas para los juegos cuyos resultados se modelaron en el punto anterior.
// APUESTA COMBINADA <<------------------------------

// hay alguna forma de decir ---> T = ResultadoCaraCruz || Int
case class ApuestaCombinada[T](apuestas: List[ApuestaSimple[T]]) extends (T => Plata) with Apuesta {
  def apply(resultado: T): Plata = apuestas.map(_.apply(resultado)).sum

  def distribucionesConMontos(): List[DistribucionGananciasConMonto] = {
    apuestas.map(d => DistribucionGananciasConMonto(d.distribucionGanancias(), d.montoApostado))
  }
}


// JUEGOS SUCESIVOS <<------------------------------
case class JuegosSucesivos(apuestas: List[Apuesta]) {

  def apply(montoInicial: Plata): DistribucionGanancias = {
    apuestas.foldLeft(DistribucionGanancias(List(SucesoConProbabilidad(PlataWrapper(montoInicial), 1.0)))) {
      (distribucion, apuesta) => apuesta match {
          case apuesta: ApuestaSimple[_] => distribucion.agregarSuceso(apuesta.distribucionGanancias(), apuesta.montoApostado)
          case apuesta: ApuestaCombinada[_] => distribucion.agregarSucesos(apuesta.distribucionesConMontos())
        }
    }
  }
}