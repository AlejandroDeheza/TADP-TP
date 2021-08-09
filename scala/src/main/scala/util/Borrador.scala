package util

/*def tieneEventoSeguro(): Boolean = distribucion.length == 1

def esEquiprobable(): Boolean = {
  import Ale.comparadorDoubles.Comparador
  val unaProbabilidad = distribucion.head.probabilidad
  distribucion.forall(_.probabilidad ~= unaProbabilidad)
}
}*/




/*
extends (TipoSuceso[_] => DistribucionProbabilidad[_])
* */




/*    case lista: List[SucesoPonderado[T]] =>
      case lista: List[T] =>
      case algo: T =>
*/




/*class GeneradorDistribuciones[T] {
  def eventoSeguro(suceso: T): DistribucionProbabilidad[T] = {
    DistribucionProbabilidad(List(SucesoConProbabilidad(suceso, 1.0)))
  }

  def equiprobable(sucesos: List[T]): DistribucionProbabilidad[T] = {
    DistribucionProbabilidad(
      for (s <- sucesos) yield SucesoConProbabilidad(s, 1.0 / sucesos.length)
    )
  }

  def ponderado(sucesos: List[SucesoPonderado[T]]): DistribucionProbabilidad[T] = {
    val pesoTotal: Int = sucesos.map(_.pesoPonderado).sum
    DistribucionProbabilidad(
      for (s <- sucesos) yield pasarASucesoProbable(s, pesoTotal)
    )
  }

  private def pasarASucesoProbable(s: SucesoPonderado[T], pesoTotal: Int): SucesoConProbabilidad[T] = {
    SucesoConProbabilidad(s.suceso, (1.0 / pesoTotal) * s.pesoPonderado)
  }
}*/





// REFACTOR IMPORTANTE DE CARA CRUZ, RULETA Y APUESTAS <<----------------------------------

/*case class CaraCruz(montoApostado: Plata, resultadoElegido: ResultadoCaraCruz) extends ApuestaSimple[ResultadoCaraCruz] {

  lazy val distribucionGanancias: DistribucionProbabilidad[Plata] =
    GeneradorDistribuciones[Plata](Equiprobables(List(pierde, ganaDoble)))

  override def apply(resultadoObtenido: ResultadoCaraCruz): Plata =
    if (resultadoObtenido == resultadoElegido) ganaDoble else pierde
}*/

/*sealed trait Juego
case object CaraCruz extends Juego
case object Ruleta extends Juego*/




/*sealed trait JugadaRuleta
case object AlRojo extends JugadaRuleta
case object AlNegro extends JugadaRuleta
case object APar extends JugadaRuleta
case object AImpar extends JugadaRuleta
case class AlNumero(numeroApostado: Int) extends JugadaRuleta
case class ADocena(docenaElegida: Docena) extends JugadaRuleta*/

/*sealed trait ResultadoRuleta// extends ResultadoDeJuego[Ruleta] { val num: Int }
case class Num(num: Int) extends ResultadoRuleta*/

/*case class Ruleta(montoApostado: Plata, jugadaElegida: JugadaRuleta) extends ApuestaSimple[ResultadoRuleta] {

  lazy val ganaTriple: Plata = montoApostado * 3
  lazy val ganaAlNumero: Plata = montoApostado * 36

  lazy val distribucionGanancias: DistribucionProbabilidad[Plata] = {
    val tupla = jugadaElegida match {
      case AlRojo | AlNegro | APar | AImpar  => (ganaDoble, 36/2, pierde, 36/2 + 1)
      case AlNumero(_)                          => (ganaAlNumero, 1, pierde., 36)
      case ADocena(_)                           => (ganaTriple, 1,   pierde, 2)
    }
    GeneradorDistribuciones[Plata](
      Ponderados(List(SucesoPonderado(tupla._1, tupla._2), SucesoPonderado(tupla._3, tupla._4)))
    )
  }

  override def apply(resultado: ResultadoRuleta): Plata = jugadaElegida match {
    case AlRojo                   => if (Rojo.contiene(resultado))             ganaDoble    else pierde
    case AlNegro                  => if (Negro.contiene(resultado))            ganaDoble    else pierde
    case APar                     => if (resultado % 2 == 0 && resultado != 0) ganaDoble    else pierde
    case AImpar                   => if (resultado % 2 != 0)                   ganaDoble    else pierde
    case AlNumero(numeroApostado) => if (resultado == numeroApostado)          ganaAlNumero else pierde
    case ADocena(docenaElegida)   => if (docenaElegida.contiene(resultado))    ganaTriple   else pierde
  }
}*/


/*trait ApuestaSimple[T] extends Apuesta[T] {
  val montoApostado: Plata
  val distribucionGanancias: DistribucionProbabilidad[Plata]
  lazy val pierde: Plata = 0
  lazy val ganaDoble: Plata = montoApostado * 2

  override def ampliarDistribucion(distInicial: DistribucionJugadas): DistribucionJugadas = {
    val sucesosNuevos = distInicial.sucesos.collect {
      case s if s.suceso >= montoApostado => distribucionGanancias.sucesos.map( s.agregar(_, this) )
      case s if s.suceso <  montoApostado => List( s.indicarQueNoJugo(this) )
    }.flatten

    distInicial.copy(sucesos = sucesosNuevos)
  }
}*/



/*
val tupla = jugada match {
      case _ : JugadaCaraCruz => /*(1,             1)*/ (1/2,         1/2)
      case _ : JugarAl        => /*(36/2,   36/2 + 1)*/ (18/37,     19/37)
      case _ : ADocena        => /*(12,           25)*/ (12/37,     25/37)
      case _ : AlNumero       => /*(1,            36)*/ ( 1/37,     36/37)
    }


    /*GeneradorDistribuciones[Plata](
      Ponderados(List(SucesoPonderado(ganar, tupla._1), SucesoPonderado(perder, tupla._2)))
    )*/
 */



/*override def ampliarDistribucion(distInicial: DistribucionApuestas): DistribucionApuestas = {
    val sucesosNuevos = distInicial.sucesos.collect {
      case s if s.valor >= montoApostado => distribucionGanancias.sucesos.map( s.indicarSiGanoOPerdio(_, this) )
      case s if s.valor <  montoApostado => List( s.indicarQueNoJugo(this) )
    }.flatten

    distInicial.copy(sucesos = sucesosNuevos)
  }*/



/*private def generarSucesosNuevos2(suceso: SucesoConEstados): List[SucesoConEstados] = {
    if (suceso.valor < montoApostado) List( suceso.indicarQueNoJugo(this) )
    else distribucionGanancias.sucesos.map{ suceso.indicarSiGanoOPerdio(_, this) }
  }*/




/// OTRO CAMBIO IMPORTANTE ----------------------------------------------------

//sealed trait JugadaRuleta extends Jugada[ResultadoRuleta]

/*case class JugarAl(modalidad: ModalidadRuleta, multiplicador: Int = 2) extends JugadaRuleta {
  lazy val probabilidadesGanarYPerder: (Double, Double) = (18/37.0, 19/37.0)
  override def cumpleCon(resultadoObtenido: ResultadoRuleta): Boolean = modalidad match {
    case Rojo   => Rojo.contiene(resultadoObtenido)
    case Negro  => Negro.contiene(resultadoObtenido)
    case Par    => resultadoObtenido % 2 == 0 && resultadoObtenido != 0
    case Impar  => resultadoObtenido % 2 != 0
  }
}

case class ADocena(docenaElegida: Docena, multiplicador: Int = 3) extends JugadaRuleta {
  lazy val probabilidadesGanarYPerder: (Double, Double) = (12/37.0, 25/37.0)
  override def cumpleCon(resultadoObtenido: ResultadoRuleta): Boolean = docenaElegida.contiene(resultadoObtenido)
}

case class AlNumero(numeroApostado: Int, multiplicador: Int = 36) extends JugadaRuleta {
  lazy val probabilidadesGanarYPerder: (Double, Double) = (1/37.0, 36/37.0)
  override def cumpleCon(resultadoObtenido: ResultadoRuleta): Boolean = numeroApostado == resultadoObtenido
}*/


/*sealed trait DocenaObject /*extends ModalidadRuleta*/ {
  val rango: Range
  def contiene(num: Int): Boolean = rango.contains(num)
}
case object PrimerDocena  extends DocenaObject { lazy val rango: Range =  1 to 12 }
case object SegundaDocena extends DocenaObject { lazy val rango: Range = 13 to 24 }
case object TercerDocena  extends DocenaObject { lazy val rango: Range = 25 to 36 }*/



//override def cumpleCon(resultadoObtenido: ResultadoCaraCruz): Boolean = resultadoObtenido == resultadoElegido




// TEST <<<-----------------------------------------

/*println(distribucion.sucesosPosibles)
      println(distribucion.sucesos.map(_.probabilidad))

      println(distribucion.sucesos.head.historial)
      println(distribucion.sucesos(1).historial)
      println(distribucion.sucesos(2).historial)
      println(distribucion.sucesos(3).historial)
      println(distribucion.sucesos(4).historial)
      println(distribucion.sucesos(5).historial)
      println(distribucion.sucesos(6).historial)
      */