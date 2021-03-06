package dominio

import util.Utils.{Plata, ResultadoDeJuego}

sealed trait EstadoApuesta
case class Gano(apuesta: ApuestaSimple[_])   extends EstadoApuesta
case class Perdio(apuesta: ApuestaSimple[_]) extends EstadoApuesta
case class NoJugo(apuesta: ApuestaSimple[_]) extends EstadoApuesta
case class Empato(apuesta: ApuestaSimple[_]) extends EstadoApuesta


sealed trait Apuesta[T] extends (T => Plata) {
  def ampliarDistribucion(distInicial: DistribucionApuestas): DistribucionApuestas
}

case class ApuestaSimple[T](montoApostado: Plata, jugada: Jugada[T]) extends Apuesta[T] {
  lazy val distribucionGanancias: DistribucionProbabilidad[Plata] = jugada.distribucionGanancias(montoApostado)

  def apply(resultadoObtenido: T): Plata = jugada(resultadoObtenido, montoApostado)

  override def ampliarDistribucion(distInicial: DistribucionApuestas): DistribucionApuestas =
    distInicial.copy(sucesos = distInicial.sucesos.flatMap(generarSucesosNuevos))

  private def generarSucesosNuevos(suceso: SucesoConEstados): List[SucesoConEstados] = {
    if (suceso.valor < montoApostado)
      List( suceso.indicarQueNoJugo(this) )
    else
      distribucionGanancias.sucesos.map{ suceso.agregarEstado(_, this) }
  }
}


// 2 - Permitir crear apuestas compuestas para los juegos cuyos resultados se modelaron en el punto anterior.

case class ApuestaCompuesta[T: ResultadoDeJuego](apuestasSimples: List[ApuestaSimple[T]]) extends Apuesta[T] {

  def apply(resultado: T): Plata = apuestasSimples.map(unaApuesta => unaApuesta(resultado)).sum

  override def ampliarDistribucion(distInicial: DistribucionApuestas): DistribucionApuestas =
    apuestasSimples.foldLeft(distInicial) { (distrib, apuestaSimple) => apuestaSimple.ampliarDistribucion(distrib) }
}


// 4 - Permitir que un jugador juegue sucesivamente varios juegos

case class ApuestasSucesivas(apuestas: List[Apuesta[_]]) extends (Plata => DistribucionApuestas) {

  def apply(montoInicial: Plata): DistribucionApuestas = {
    val semilla = DistribucionApuestas(List(SucesoConEstados(montoInicial, 1.0, List())), montoInicial)
    apuestas.foldLeft(semilla) { (distribucion, unaApuesta) => unaApuesta.ampliarDistribucion(distribucion) }
  }
}
