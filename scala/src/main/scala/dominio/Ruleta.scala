package dominio

import util.Utils.{Plata, ResultadoRuleta}

sealed trait JugadaRuleta
case class AlRojo() extends JugadaRuleta
case class AlNegro() extends JugadaRuleta
case class APar() extends JugadaRuleta
case class AImpar() extends JugadaRuleta
case class AlNumero(numeroApostado: Int) extends JugadaRuleta
case class ADocena(docenaElegida: Docena) extends JugadaRuleta

case class Ruleta(montoApostado: Plata, jugadaElegida: JugadaRuleta) extends (ResultadoRuleta => Plata)
  with JuegoSimple[ResultadoRuleta] {

  lazy val ganaTriple: Plata = montoApostado * 3
  lazy val ganaAlNumero: Plata = montoApostado * 36
  lazy val distribucionGanancias: DistribucionProbabilidad[Plata] = {
    val c = jugadaElegida match {
      case AlRojo() =>    (ganaDoble, Rojo.valores.length,  pierde, Negro.valores.length + 1)
      case AlNegro() =>   (ganaDoble, Negro.valores.length, pierde, Rojo.valores.length + 1)
      case APar() =>      (ganaDoble, 36/2,                 pierde, 36/2 + 1)
      case AImpar() =>    (ganaDoble, 36/2,                 pierde, 36/2 + 1)
      case AlNumero(_) => (ganaAlNumero, 1,                 pierde, 36)
      case ADocena(_) =>  (ganaTriple, 1,                   pierde, 2)
    }
    new GeneradorDistribuciones[Plata]().ponderado(
      List(SucesoPonderado(c._1, c._2), SucesoPonderado(c._3, c._4))
    )
  }

  override def apply(resultado: ResultadoRuleta): Plata = jugadaElegida match {
    case AlRojo() =>                 if (Rojo.contiene(resultado))             ganaDoble else pierde
    case AlNegro() =>                if (Negro.contiene(resultado))            ganaDoble else pierde
    case APar() =>                   if (resultado % 2 == 0 && resultado != 0) ganaDoble else pierde
    case AImpar() =>                 if (resultado % 2 != 0)                   ganaDoble else pierde
    case AlNumero(numeroApostado) => if (resultado == numeroApostado)          ganaAlNumero else pierde
    case ADocena(docenaElegida) =>   if (docenaElegida.contiene(resultado))    ganaTriple else pierde
  }
}

sealed trait Color {
  val valores: List[Int]
  def contiene(num: Int): Boolean = valores.contains(num)
}
case object Rojo extends Color {
  lazy val valores: List[Int] = List(1, 3, 5, 7, 9, 12, 14, 16, 18, 19, 21, 23, 25, 27, 30, 32, 34, 36)
}
case object Negro extends Color {
  lazy val valores: List[Int] = List(2, 4, 6, 8, 10, 11, 13, 15, 17, 20, 22, 24, 26, 28, 29, 31, 33, 35)
}

sealed trait Docena {
  val rango: Range
  def contiene(num: Int): Boolean = rango.contains(num)
}
case object PrimerDocena  extends Docena { lazy val rango: Range = 1 to 12 }
case object SegundaDocena extends Docena { lazy val rango: Range = 13 to 24 }
case object TercerDocena  extends Docena { lazy val rango: Range = 25 to 36 }
