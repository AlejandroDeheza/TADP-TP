package Ale

import Ale.Utils.{Plata, ResultadoDeJuego, head, last}

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
case class ApuestaCombinada[T : ResultadoDeJuego](apuestas: List[ApuestaSimple[T]]) extends (T => Plata) with Apuesta {

  def apply(resultado: T): Plata = ( for (a <- apuestas) yield a(resultado) ).sum

  def distribucionesConMontos(): List[DistribucionGananciasConMonto] = {
    for (a <- apuestas) yield DistribucionGananciasConMonto(a.distribucionGanancias(), a.montoApostado)
  }
}


// JUEGOS SUCESIVOS <<------------------------------
case class JuegosSucesivos(apuestas: List[Apuesta]) {

  def apply(montoInicial: Plata): DistribucionProbabilidad[Plata] = {
    apuestas.foldLeft(DistribucionProbabilidad[Plata](List(SucesoConProbabilidad(montoInicial, 1.0)))) {
      (distribucion, apuesta) => apuesta match {
          case apuesta: ApuestaSimple[_] => agregarSuceso(distribucion, apuesta.distribucionGanancias(), apuesta.montoApostado)
          case apuesta: ApuestaCombinada[_] => agregarSucesos(distribucion, apuesta.distribucionesConMontos())
        } // TODO simplifica esto un poquito, Capaz podes agregar logica al trait "Apuesta"
    }
  }

  // TODO: TESTEA BIEN ESTO DE ABAJO, ES EL CORE DEL TP
  // TODO: fijate si es facil agregar lo que decia sobre el historial de cierto suceso
  private def agregarSuceso(distInicial: DistribucionProbabilidad[Plata], distribDe2Sucesos: DistribucionProbabilidad[Plata],
                    montoApostado: Plata): DistribucionProbabilidad[Plata] = {

    val sucesosNormales =     for (s <- distInicial.distribucion if s.suceso >= montoApostado) yield s
    val sucesosConPocaPlata = for (s <- distInicial.distribucion if s.suceso < montoApostado)  yield s
    
    val sucesosNuevos =  for (s <- sucesosNormales) yield generarSuceso(s, montoApostado, distribDe2Sucesos, head)
    val sucesosNuevos2 = for (s <- sucesosNormales) yield generarSuceso(s, montoApostado, distribDe2Sucesos, last)

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