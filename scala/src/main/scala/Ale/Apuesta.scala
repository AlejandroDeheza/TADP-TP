package Ale

import Ale.Utils.{Plata, head, last}

//trait ResultadoDeJuego

sealed trait Apuesta
// Tomo a la apuesta y la jugada como el mismo objeto ---> Apuesta

trait ApuestaSimple[T] extends Apuesta {
  val montoApostado: Plata
  def apply(resultado: T): Plata
  def perder(): Plata = 0
  def ganarDoble(): Plata = montoApostado * 2
  def distribucionGanancias(): DistribucionProbabilidad[Plata]
}


// 2 - Permitir crear apuestas compuestas para los juegos cuyos resultados se modelaron en el punto anterior.
// APUESTA COMBINADA <<------------------------------

// hay alguna forma de decir ---> T = ResultadoCaraCruz || Int // TODO: REVISAR
case class ApuestaCombinada[T](apuestas: List[ApuestaSimple[T]]) extends (T => Plata) with Apuesta {
  def apply(resultado: T): Plata = apuestas.map(_.apply(resultado)).sum

  def distribucionesConMontos(): List[DistribucionGananciasConMonto] = {
    apuestas.map(d => DistribucionGananciasConMonto(d.distribucionGanancias(), d.montoApostado))
  }
}


// JUEGOS SUCESIVOS <<------------------------------
case class JuegosSucesivos(apuestas: List[Apuesta]) {

  def apply(montoInicial: Plata): DistribucionProbabilidad[Plata] = {
    apuestas.foldLeft(DistribucionProbabilidad[Plata](List(SucesoConProbabilidad(montoInicial, 1.0)))) {
      (distribucion, apuesta) => apuesta match {
          case apuesta: ApuestaSimple[_] => agregarSuceso(distribucion, apuesta.distribucionGanancias(), apuesta.montoApostado)
          case apuesta: ApuestaCombinada[_] => agregarSucesos(distribucion, apuesta.distribucionesConMontos())
        }
    }
  }

  private def agregarSuceso(distInicial: DistribucionProbabilidad[Plata], distribDe2Sucesos: DistribucionProbabilidad[Plata],
                    montoApostado: Plata): DistribucionProbabilidad[Plata] = {
    val sucesosNormales = distInicial.distribucion.filter(s => s.suceso >= montoApostado)
    val sucesosConPocaPlata = distInicial.distribucion.filter(s => s.suceso < montoApostado)

    val sucesosNuevos = sucesosNormales.map(s => generarSuceso(s, montoApostado, distribDe2Sucesos, head))
    val sucesosNuevos2 = sucesosNormales.map(s => generarSuceso(s, montoApostado, distribDe2Sucesos, last))

    DistribucionProbabilidad[Plata](sucesosNuevos ++ sucesosNuevos2 ++ sucesosConPocaPlata)
  }

  private def agregarSucesos(distInicial: DistribucionProbabilidad[Plata], distribucionesConMontos: List[DistribucionGananciasConMonto])
  : DistribucionProbabilidad[Plata] = {
    distribucionesConMontos.foldLeft(distInicial.copy()) { (distrib, distribConMontos) =>
      agregarSuceso(distrib, distribConMontos.distribucion, distribConMontos.montoApostado)
    }
  }

  private def generarSuceso(s: SucesoConProbabilidad[Plata], montoApostado: Plata, d: DistribucionProbabilidad[Plata],
                            f: Iterable[SucesoConProbabilidad[Plata]] => SucesoConProbabilidad[Plata])
  : SucesoConProbabilidad[Plata] = {
    val montoInicial = s.suceso
    val ganancia = f(d.copy().distribucion).suceso
    val multiplicacionProbabilidades = s.probabilidad * f(d.copy().distribucion).probabilidad

    SucesoConProbabilidad( montoInicial - montoApostado + ganancia, multiplicacionProbabilidades )
  }
}