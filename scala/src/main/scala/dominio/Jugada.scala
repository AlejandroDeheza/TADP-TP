package dominio

import util.Utils.ResultadoRuleta

sealed trait ResultadoCaraCruz
case object Cara extends ResultadoCaraCruz
case object Cruz extends ResultadoCaraCruz

sealed trait Jugada[T] {
  val multiplicador: Int
  val multiplicadorSiPierde: Int = 0
  def cumpleCon(resultadoObtenido: T): Boolean
}

case class JugadaCaraCruz(resultadoElegido: ResultadoCaraCruz, multiplicador: Int = 2) extends Jugada[ResultadoCaraCruz] {
  override def cumpleCon(resultadoObtenido: ResultadoCaraCruz): Boolean = resultadoObtenido == resultadoElegido
}


// RULETA <<--------------------------------------------------------
sealed trait JugadaRuleta extends Jugada[ResultadoRuleta]

case class JugarAl(modalidad: ModalidadRuleta, multiplicador: Int = 2) extends JugadaRuleta {
  override def cumpleCon(resultadoObtenido: ResultadoRuleta): Boolean = modalidad match {
    case Rojo   => Rojo.contiene(resultadoObtenido)
    case Negro  => Negro.contiene(resultadoObtenido)
    case Par    => resultadoObtenido % 2 == 0 && resultadoObtenido!= 0
    case Impar  => resultadoObtenido % 2 != 0
  }
}
case class ADocena(docenaElegida: Docena, multiplicador: Int = 3) extends JugadaRuleta {
  override def cumpleCon(resultadoObtenido: ResultadoRuleta): Boolean = docenaElegida.contiene(resultadoObtenido)
}
case class AlNumero(numeroApostado: Int, multiplicador: Int = 36) extends JugadaRuleta {
  override def cumpleCon(resultadoObtenido: ResultadoRuleta): Boolean = numeroApostado == resultadoObtenido
}


sealed trait ModalidadRuleta
case object Par extends ModalidadRuleta
case object Impar extends ModalidadRuleta

sealed trait Color extends ModalidadRuleta {
  val valores: List[Int]
  def contiene(num: Int): Boolean = valores.contains(num)
}
case object Rojo extends Color {
  lazy val valores: List[Int] = List(1, 3, 5, 7,  9, 12, 14, 16, 18, 19, 21, 23, 25, 27, 30, 32, 34, 36)
}
case object Negro extends Color {
  lazy val valores: List[Int] = List(2, 4, 6, 8, 10, 11, 13, 15, 17, 20, 22, 24, 26, 28, 29, 31, 33, 35)
}

sealed trait Docena {
  val rango: Range
  def contiene(num: Int): Boolean = rango.contains(num)
}
case object PrimerDocena  extends Docena { lazy val rango: Range =  1 to 12 }
case object SegundaDocena extends Docena { lazy val rango: Range = 13 to 24 }
case object TercerDocena  extends Docena { lazy val rango: Range = 25 to 36 }


// JUEGO <<----------------------------------
trait Juego
case object CaraCruz extends Juego {
  lazy val valores: List[ResultadoCaraCruz] = List(Cara, Cruz)
  lazy val distribucionResultados: DistribucionProbabilidad[ResultadoCaraCruz] =
    GeneradorDistribuciones(Equiprobables(valores))
}
case object Ruleta extends Juego {
  lazy val valores: List[ResultadoRuleta] = (0 to 36).toList
  lazy val distribucionResultados: DistribucionProbabilidad[ResultadoRuleta] =
    GeneradorDistribuciones(Equiprobables(valores))
}