package dominio

import util.Utils.ResultadoRuleta

// 1 - Modelar los resultados de juegos posibles y las jugadas mencionadas que se pueden hacer sobre ellos.

sealed trait Jugada[T] {
  val multiplicador: Int
  val multiplicadorSiPierde: Int = 0
  val probabilidadesGanarYPerder: (Double, Double)
  def cumpleCon(resultadoObtenido: T): Boolean
}


// CARA CRUZ <<-----------------------------------------------------
sealed trait ResultadoCaraCruz
case object Cara extends ResultadoCaraCruz
case object Cruz extends ResultadoCaraCruz

case class CaraCruz(resultadoElegido: ResultadoCaraCruz, multiplicador: Int = 2) extends Jugada[ResultadoCaraCruz] {
  lazy val probabilidadesGanarYPerder: (Double, Double) = (1/2.0, 1/2.0)

  override def cumpleCon(resultadoObtenido: ResultadoCaraCruz): Boolean = resultadoObtenido == resultadoElegido
}


// RULETA <<--------------------------------------------------------
case class Ruleta(modalidadElegida: ModalidadRuleta) extends Jugada[ResultadoRuleta] {
  lazy val multiplicador: Int = modalidadElegida.mult
  lazy val probabilidadesGanarYPerder: (Double, Double) = modalidadElegida.probGanarYPerder

  override def cumpleCon(resultadoObtenido: ResultadoRuleta): Boolean = modalidadElegida match {
    case _: Rojo  | Rojo        => Rojo.contiene(resultadoObtenido)
    case _: Negro | Negro       => Negro.contiene(resultadoObtenido)
    case _: Par   | Par         => resultadoObtenido % 2 == 0 && resultadoObtenido != 0
    case _: Impar | Impar       => resultadoObtenido % 2 != 0
    case docena: Docena         => docena.contiene(resultadoObtenido)
    case Al(numeroApostado, _)  => numeroApostado == resultadoObtenido
  }
}


// JUEGO (companion objects de jugadas para obtener las distribuciones de resultados) <<----------------------------------
trait Juego[T] {
  val valores: List[T]
  lazy val distribucionResultados: DistribucionProbabilidad[T] =
    GeneradorDistribuciones(Equiprobables(valores))
}
case object CaraCruz extends Juego[ResultadoCaraCruz] { lazy val valores: List[ResultadoCaraCruz] = List(Cara, Cruz) }
case object Ruleta   extends Juego[ResultadoRuleta]   { lazy val valores: List[ResultadoRuleta] = (0 to 36).toList }