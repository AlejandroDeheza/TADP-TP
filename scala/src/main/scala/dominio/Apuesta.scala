package dominio

import util.Utils.{Plata, ResultadoDeJuego}

sealed trait EstadoApuesta
case class Gano(apuesta: ApuestaSimple[_])   extends EstadoApuesta
case class Perdio(apuesta: ApuestaSimple[_]) extends EstadoApuesta
case class NoJugo(apuesta: ApuestaSimple[_]) extends EstadoApuesta


sealed trait Apuesta[T] extends (T => Plata) {
  def ampliarDistribucion(distInicial: DistribucionApuestas): DistribucionApuestas
}

case class ApuestaSimple[T](montoApostado: Plata, jugada: Jugada[T]) extends Apuesta[T] {
  lazy val ganar: Plata = jugada.multiplicador * montoApostado
  lazy val perder: Plata = jugada.multiplicadorSiPierde * montoApostado

  lazy val distribucionGanancias: DistribucionProbabilidad[Plata] = {
    val tupla = jugada match {
      case _ : JugadaCaraCruz => (  1/2.0,    1/2.0)
      case _ : JugarAl        => (18/37.0,  19/37.0)
      case _ : ADocena        => (12/37.0,  25/37.0)
      case _ : AlNumero       => ( 1/37.0,  36/37.0)
    }
    DistribucionProbabilidad(List(SucesoConProbabilidad(ganar, tupla._1), SucesoConProbabilidad(perder, tupla._2)))
  }

  def apply(resultadoObtenido: T): Plata = if (jugada.cumpleCon(resultadoObtenido)) ganar else perder

  override def ampliarDistribucion(distInicial: DistribucionApuestas): DistribucionApuestas = {
    val sucesosNuevos = distInicial.sucesos.collect {
      case s if s.valor >= montoApostado => distribucionGanancias.sucesos.map( s.indicarQueSiJugo(_, this) )
      case s if s.valor <  montoApostado => List( s.indicarQueNoJugo(this) )
    }.flatten

    distInicial.copy(sucesos = sucesosNuevos)
  }
}


// 2 - Permitir crear apuestas compuestas para los juegos cuyos resultados se modelaron en el punto anterior.
// APUESTA COMPUESTA <<------------------------------
case class ApuestaCompuesta[T: ResultadoDeJuego](apuestasSimples: List[ApuestaSimple[T]]) extends Apuesta[T] {

  def apply(resultado: T): Plata = apuestasSimples.map(unaApuesta => unaApuesta(resultado)).sum

  override def ampliarDistribucion(distInicial: DistribucionApuestas): DistribucionApuestas =
    apuestasSimples.foldLeft(distInicial){ (distrib, apuestaSimple) => apuestaSimple.ampliarDistribucion(distrib) }
}


// 4 - Permitir que un jugador juegue sucesivamente varios juegos
// APUESTAS SUCESIVAS <<---------------------------------------
case class ApuestasSucesivas(apuestas: List[Apuesta[_]]) extends (Plata => DistribucionApuestas) {

  def apply(montoInicial: Plata): DistribucionApuestas = {
    val semilla = DistribucionApuestas(List(SucesoConEstados(montoInicial, 1.0, List())), montoInicial)
    apuestas.foldLeft(semilla) { (distribucion, unaApuesta) => unaApuesta.ampliarDistribucion(distribucion) }
  } // TODO: TESTEAR BIEN ESTO
}
