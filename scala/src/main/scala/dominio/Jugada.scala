package dominio

import util.Utils.{Plata, ResultadoRuleta}

// 1 - Modelar los resultados de juegos posibles y las jugadas mencionadas que se pueden hacer sobre ellos.

sealed trait Jugada[T] extends ((T, Plata) => Plata){
  val multiplicador: Int
  val probabilidadesGanarPerderEmpatar: (Double, Double, Double)
  lazy val perder: Plata = 0

  def ganar(montoApostado: Plata): Plata = montoApostado * multiplicador

  def distribucionGanancias(montoApostado: Plata): DistribucionProbabilidad[Plata] = {
    DistribucionProbabilidad(List(
      SucesoConProbabilidad(ganar(montoApostado), probabilidadesGanarPerderEmpatar._1),
      SucesoConProbabilidad(perder, probabilidadesGanarPerderEmpatar._2),
      SucesoConProbabilidad(montoApostado, probabilidadesGanarPerderEmpatar._3)
    ))
  }
}


// CARA CRUZ <<-----------------------------------------------------
sealed trait ResultadoCaraCruz
case object Cara extends ResultadoCaraCruz
case object Cruz extends ResultadoCaraCruz

case class CaraCruz(resultadoElegido: ResultadoCaraCruz, multiplicador: Int = 2) extends Jugada[ResultadoCaraCruz] {
  lazy val probabilidadesGanarPerderEmpatar: (Double, Double, Double) = (1/2.0, 1/2.0, 0.0)

  override def apply(resultadoObtenido: ResultadoCaraCruz, montoApostado: Plata): Plata =
    if (resultadoObtenido == resultadoElegido) ganar(montoApostado) else perder
}


// RULETA <<--------------------------------------------------------
case class Ruleta(modalidadElegida: ModalidadRuleta) extends Jugada[ResultadoRuleta] {
  lazy val multiplicador: Int = modalidadElegida.mult
  lazy val probabilidadesGanarPerderEmpatar: (Double, Double, Double) = modalidadElegida.probGanarPerderEmpatar

  override def apply(resultadoObtenido: ResultadoRuleta, montoApostado: Plata): Plata = {
    val hayAcierto: Boolean = modalidadElegida match {
      case _: Rojo  | Rojo        => Rojo.contiene(resultadoObtenido)
      case _: Negro | Negro       => Negro.contiene(resultadoObtenido)
      case _: Par   | Par         => resultadoObtenido % 2 == 0 && resultadoObtenido != 0
      case _: Impar | Impar       => resultadoObtenido % 2 != 0
      case docena: Docena         => docena.contiene(resultadoObtenido)
      case Al(numeroApostado, _)  => numeroApostado == resultadoObtenido
    }
    if (hayAcierto) ganar(montoApostado) else perder
  }
}


// PIEDRA PAPEL O TIJERA <<--------------------------------
case class PiedraPapelTijera(resultadoElegido: ResultadoPPT, multiplicador: Int = 2) extends Jugada[ResultadoPPT] {
  lazy val probabilidadesGanarPerderEmpatar: (Double, Double, Double) = (resultadoElegido.leGanaA.probabilidadDeObtencion,
    resultadoElegido.leGanaA.leGanaA.probabilidadDeObtencion, resultadoElegido.probabilidadDeObtencion)

  override def apply(resultadoObtenido: ResultadoPPT, montoApostado: Plata): Plata =
    if (resultadoElegido.leGanaA == resultadoObtenido) ganar(montoApostado)
    else if(resultadoElegido == resultadoObtenido) montoApostado
    else perder
}


// JUEGO (companion objects de jugadas para obtener las distribuciones de resultados) <<----------------------------------
sealed trait Juego[T] {
  val valores: List[T]
  lazy val distribucionResultados: DistribucionProbabilidad[T] =
    GeneradorDistribuciones(Equiprobables(valores))
}
case object CaraCruz extends Juego[ResultadoCaraCruz] { lazy val valores: List[ResultadoCaraCruz] = List(Cara, Cruz) }
case object Ruleta   extends Juego[ResultadoRuleta]   { lazy val valores: List[ResultadoRuleta] = (0 to 36).toList }

case object PiedraPapelTijera extends Juego[ResultadoPPT] {
  lazy val valores: List[ResultadoPPT] = List(Piedra, Papel, Tijera)
  override lazy val distribucionResultados: DistribucionProbabilidad[ResultadoPPT] = {
    val ponderados = List[SucesoPonderado[ResultadoPPT]](
      SucesoPonderado(Piedra, 40), SucesoPonderado(Tijera, 35), SucesoPonderado(Papel, 25)
    )
    GeneradorDistribuciones(Ponderados(ponderados))
  }
}