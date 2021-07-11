package Ale

import Ale.Utils.Plata

case class ResultadoRuleta(numeroResultado: Int) extends ResultadoDeJuego {
  require(numeroResultado >= 0 && numeroResultado < 37)
}

sealed trait ApuestaRuleta extends (ResultadoRuleta => Plata) with ApuestaSimple {
  val sucesoGanar: SucesoPonderado
  val sucesoPerder: SucesoPonderado
  def ganarTriple(): Plata = montoApostado * 3
  def ganarAlNumero(): Plata = montoApostado * 36
  def distribucionGanancias(): DistribucionGanancias = {
    GeneradorDistribucionesGanancias().Ponderado(List(sucesoGanar, sucesoPerder))
  }
}

case class JugarAlRojo(montoApostado: Plata) extends ApuestaRuleta {
  val sucesoGanar: SucesoPonderado = SucesoPonderado(PlataWrapper(ganarDoble()), Rojo.valores.length)
  val sucesoPerder: SucesoPonderado = SucesoPonderado(PlataWrapper(perder()), Negro.valores.length + 1)

  override def apply(resultado: ResultadoRuleta): Plata = resultado match {
    case ResultadoRuleta(numeroResultado) if Rojo.contiene(numeroResultado) => ganarDoble()
    case _ => perder()
  }
}

case class JugarAlNegro(montoApostado: Plata) extends ApuestaRuleta {
  val sucesoGanar: SucesoPonderado = SucesoPonderado(PlataWrapper(ganarDoble()), Negro.valores.length)
  val sucesoPerder: SucesoPonderado = SucesoPonderado(PlataWrapper(perder()), Rojo.valores.length + 1)

  override def apply(resultado: ResultadoRuleta): Plata = resultado match {
    case ResultadoRuleta(numeroResultado) if Negro.contiene(numeroResultado) => ganarDoble()
    case _ => perder()
  }
}

case class JugarAlNumero(montoApostado: Plata, numeroApostado: Plata) extends ApuestaRuleta {
  val sucesoGanar: SucesoPonderado = SucesoPonderado(PlataWrapper(ganarAlNumero()), 1)
  val sucesoPerder: SucesoPonderado = SucesoPonderado(PlataWrapper(perder()), 36)

  override def apply(resultado: ResultadoRuleta): Plata = resultado match {
    case ResultadoRuleta(resultado) if resultado == numeroApostado => ganarAlNumero()
    case _ => perder()
  }
}

case class JugarAPar(montoApostado: Plata) extends ApuestaRuleta {
  val sucesoGanar: SucesoPonderado = SucesoPonderado(PlataWrapper(ganarDoble()), 36 / 2)
  val sucesoPerder: SucesoPonderado = SucesoPonderado(PlataWrapper(perder()), 36 / 2 + 1)

  override def apply(resultado: ResultadoRuleta): Plata = resultado match {
    case ResultadoRuleta(0) => perder()
    case ResultadoRuleta(resultado) if resultado % 2 == 0 => ganarDoble()
    case _ => perder()
  }
}

case class JugarAImpar(montoApostado: Plata) extends ApuestaRuleta {
  val sucesoGanar: SucesoPonderado = SucesoPonderado(PlataWrapper(ganarDoble()), 36 / 2)
  val sucesoPerder: SucesoPonderado = SucesoPonderado(PlataWrapper(perder()), 36 / 2 + 1)

  override def apply(resultado: ResultadoRuleta): Plata = resultado match {
    case ResultadoRuleta(resultado) if resultado % 2 != 0 => ganarDoble()
    case _ => perder()
  }
}

case class JugarADocena(montoApostado: Plata, docenaElegida: Docena) extends ApuestaRuleta {
  val sucesoGanar: SucesoPonderado = SucesoPonderado(PlataWrapper(ganarTriple()), 1)
  val sucesoPerder: SucesoPonderado = SucesoPonderado(PlataWrapper(perder()), 2)

  override def apply(resultado: ResultadoRuleta): Plata = resultado match {
    case ResultadoRuleta(resultado) if docenaElegida.contiene(resultado) => ganarTriple()
    case _ => perder()
  }
}

sealed trait Color {
  val valores: List[Int]
  def contiene(num: Int): Boolean
}
case object Rojo extends Color {
  val valores: List[Int] = List(1, 3, 5, 7, 9, 12, 14, 16, 18, 19, 21, 23, 25, 27, 30, 32, 34, 36)
  override def contiene(num: Int): Boolean = valores.contains(num)
}
case object Negro extends Color {
  val valores: List[Int] = List(2, 4, 6, 8, 10, 11, 13, 15, 17, 20, 22, 24, 26, 28, 29, 31, 33, 35)
  override def contiene(num: Int): Boolean = valores.contains(num)
}

sealed trait Docena {
  def contiene(num: Int): Boolean
}
case object PrimerDocena extends Docena {
  override def contiene(num: Int): Boolean = num >= 1 && num <= 12
}
case object SegundaDocena extends Docena {
  override def contiene(num: Int): Boolean = num >= 13 && num <= 24
}
case object TercerDocena extends Docena {
  override def contiene(num: Int): Boolean = num >= 25 && num <= 36
}
