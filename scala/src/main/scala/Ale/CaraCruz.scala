package Ale

import Ale.Utils.Plata

sealed trait ResultadoCaraCruz extends ResultadoDeJuego
case object Cara extends ResultadoCaraCruz
case object Cruz extends ResultadoCaraCruz

sealed trait ApuestaCaraCruz extends (ResultadoCaraCruz => Plata) with ApuestaSimple {
  def distribucionGanancias(): DistribucionGanancias = {
    GeneradorDistribucionesGanancias().Equiprobable(List(PlataWrapper(perder()), PlataWrapper(ganarDoble())))
  }
}

case class JugarACara(montoApostado: Plata) extends ApuestaCaraCruz {
  override def apply(resultado: ResultadoCaraCruz): Plata = resultado match {
    case Cara => ganarDoble()
    case _ => perder()
  }
}

case class JugarACruz(montoApostado: Plata) extends ApuestaCaraCruz {
  override def apply(resultado: ResultadoCaraCruz): Plata = resultado match {
    case Cruz => ganarDoble()
    case _ => perder()
  }
}
