package Ale

import Ale.Utils.{Plata, ResultadoRuleta}

sealed trait JugadaRuleta
case class AlRojo() extends JugadaRuleta
case class AlNegro() extends JugadaRuleta
case class APar() extends JugadaRuleta
case class AImpar() extends JugadaRuleta
case class AlNumero(numeroApostado: Int) extends JugadaRuleta
case class ADocena(docenaElegida: Docena) extends JugadaRuleta

case class Ruleta(montoApostado: Plata, jugadaElegida: JugadaRuleta) extends (ResultadoRuleta => Plata)
  with ApuestaSimple[ResultadoRuleta] {

  override def apply(resultado: ResultadoRuleta): Plata = jugadaElegida match {
    case AlRojo() => if (Rojo.contiene(resultado)) ganarDoble() else perder()
    case AlNegro() => if (Negro.contiene(resultado)) ganarDoble() else perder()
    case APar() => if (resultado % 2 == 0 && resultado != 0) ganarDoble() else perder()
    case AImpar() => if (resultado % 2 != 0) ganarDoble() else perder()
    case AlNumero(numeroApostado) => if (resultado == numeroApostado) ganarAlNumero() else perder()
    case ADocena(docenaElegida) => if (docenaElegida.contiene(resultado)) ganarTriple() else perder()
  }

  def distribucionGanancias(): DistribucionProbabilidad[Plata] = {
    new GeneradorDistribuciones[Plata]().Ponderado(
      jugadaElegida match {
        case AlRojo() => List(SucesoPonderado(ganarDoble(), Rojo.valores.length),
          SucesoPonderado(perder(), Negro.valores.length + 1))
        case AlNegro() => List(SucesoPonderado(ganarDoble(), Negro.valores.length),
          SucesoPonderado(perder(), Rojo.valores.length + 1))
        case APar() => List(SucesoPonderado(ganarDoble(), 36 / 2), SucesoPonderado(perder(), 36 / 2 + 1))
        case AImpar() => List(SucesoPonderado(ganarDoble(), 36 / 2), SucesoPonderado(perder(), 36 / 2 + 1))
        case AlNumero(_) => List(SucesoPonderado(ganarAlNumero(), 1), SucesoPonderado(perder(), 36))
        case ADocena(_) => List(SucesoPonderado(ganarTriple(), 1), SucesoPonderado(perder(), 2))
      }
    )
  }

  def ganarTriple(): Plata = montoApostado * 3
  def ganarAlNumero(): Plata = montoApostado * 36
}

sealed trait Color {
  val valores: List[Int]
  def contiene(num: Int): Boolean = valores.contains(num)
}
case object Rojo extends Color {
  val valores: List[Int] = List(1, 3, 5, 7, 9, 12, 14, 16, 18, 19, 21, 23, 25, 27, 30, 32, 34, 36)
}
case object Negro extends Color {
  val valores: List[Int] = List(2, 4, 6, 8, 10, 11, 13, 15, 17, 20, 22, 24, 26, 28, 29, 31, 33, 35)
}

sealed trait Docena {
  val rango: Range
  def contiene(num: Int): Boolean = rango.contains(num)
}
case object PrimerDocena extends Docena { val rango: Range = 1 to 12 }
case object SegundaDocena extends Docena { val rango: Range = 13 to 24 }
case object TercerDocena extends Docena { val rango: Range = 25 to 36 }
