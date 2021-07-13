package Ale

import Ale.Utils.Plata

sealed trait ResultadoCaraCruz
case object Cara extends ResultadoCaraCruz
case object Cruz extends ResultadoCaraCruz

case class CaraCruz(montoApostado: Plata, resultadoElegido: ResultadoCaraCruz) extends (ResultadoCaraCruz => Plata)
  with JuegoSimple[ResultadoCaraCruz] {

  override def apply(resultadoObtenido: ResultadoCaraCruz): Plata = {
    if (resultadoObtenido == resultadoElegido) ganarDoble() else perder()
  }

  def distribucionGanancias(): DistribucionProbabilidad[Plata] = {
    new GeneradorDistribuciones().Equiprobable(List(perder(), ganarDoble()))
  }
}

