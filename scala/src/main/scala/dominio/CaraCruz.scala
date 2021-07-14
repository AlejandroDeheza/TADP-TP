package dominio

import util.Utils.Plata

sealed trait ResultadoCaraCruz
case object Cara extends ResultadoCaraCruz
case object Cruz extends ResultadoCaraCruz

case class CaraCruz(montoApostado: Plata, resultadoElegido: ResultadoCaraCruz) extends JuegoSimple[ResultadoCaraCruz] {

  lazy val distribucionGanancias: DistribucionProbabilidad[Plata] =
    GeneradorDistribuciones[Plata](Equiprobables(List(pierde, ganaDoble)))

  override def apply(resultadoObtenido: ResultadoCaraCruz): Plata =
    if (resultadoObtenido == resultadoElegido) ganaDoble else pierde
}

