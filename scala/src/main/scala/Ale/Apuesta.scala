package Ale

import Ale.Utils.Plata

sealed trait SucesoGenerico
trait ResultadoDeJuego extends SucesoGenerico
case class PlataWrapper(monto: Plata) extends SucesoGenerico  // TODO: REVISAR

sealed trait Apuesta
// Tomo a la apuesta y la jugada como el mismo objeto ---> Apuesta

trait ApuestaSimple extends Apuesta {
  val montoApostado: Plata
  def perder(): Plata = 0
  def ganarDoble(): Plata = montoApostado * 2
  def distribucionGanancias(): DistribucionGanancias
}


// 2 - Permitir crear apuestas compuestas para los juegos cuyos resultados se modelaron en el punto anterior.

// APUESTA COMBINADA <<------------------------------
sealed trait ApuestaCombinada extends Apuesta {
  val apuestas: List[ApuestaSimple]

  def distribucionesConMontos(): List[DistribucionGananciasConMonto] = {
    apuestas.map(d => DistribucionGananciasConMonto(d.distribucionGanancias(), d.montoApostado))
  }
}

case class CaraCruzCombinada(apuestas: List[ApuestaCaraCruz]) extends (ResultadoCaraCruz => Plata) with ApuestaCombinada {
  override def apply(resultado: ResultadoCaraCruz): Plata = apuestas.map(_.apply(resultado)).sum
}

case class RuletaCombinada(apuestas: List[ApuestaRuleta]) extends (ResultadoRuleta => Plata) with ApuestaCombinada {
  override def apply(resultado: ResultadoRuleta): Plata = apuestas.map(_.apply(resultado)).sum
}


// JUEGOS SUCESIVOS <<------------------------------
case class JuegosSucesivos(apuestas: List[Apuesta]) {

  def apply(montoInicial: Plata): DistribucionGanancias = {
    apuestas.foldLeft(DistribucionGanancias(List(SucesoConProbabilidad(PlataWrapper(montoInicial), 1.0)))) {
      (distribucion, apuesta) => apuesta match {
          case apuesta: ApuestaSimple => distribucion.agregarSuceso(apuesta.distribucionGanancias(), apuesta.montoApostado)
          case apuesta: ApuestaCombinada => distribucion.agregarSucesos(apuesta.distribucionesConMontos())
        }
    }
  }
}