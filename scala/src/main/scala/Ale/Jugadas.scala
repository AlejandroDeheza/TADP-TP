package Ale

sealed trait SucesoGenerico

sealed trait ResultadoDeJuego extends SucesoGenerico

case class Ganancia(monto: Int) extends SucesoGenerico {
  def +(g: Ganancia): Ganancia = Ganancia(g.monto + monto)
}

sealed trait Jugada

sealed trait JugadaSimple extends Jugada {
  val montoApostado: Int
  def perder(): Ganancia = Ganancia(0)
  def ganarDoble(): Ganancia = Ganancia(montoApostado * 2)
} // tomo a "jugada" y "apuesta" como un mismo objeto --> Jugada




// 1 - Modelar los resultados de juegos posibles
// para la ruleta y para ‘cara o cruz’
// y las jugadas mencionadas que se pueden hacer sobre ellos.

sealed trait ResultadoCaraCruz extends ResultadoDeJuego
case object Cara extends ResultadoCaraCruz
case object Cruz extends ResultadoCaraCruz

sealed trait JugadaCaraCruz extends (ResultadoCaraCruz => Ganancia) with JugadaSimple {
  def distribucionDeGanancias(): DistribucionProbabilidad = {
    GeneradorDistribuciones().distribucionEquiprobable(List(perder(), ganarDoble()))
  }
}

case class DuplicarSiSaleCara(montoApostado: Int) extends JugadaCaraCruz {
  override def apply(resultado: ResultadoCaraCruz): Ganancia = resultado match {
    case Cara => ganarDoble()
    case _ => perder()
  }
}

case class DuplicarSiSaleCruz(montoApostado: Int) extends JugadaCaraCruz {
  override def apply(resultado: ResultadoCaraCruz): Ganancia = resultado match {
    case Cruz => ganarDoble()
    case _ => perder()
  }
}



// RULETA <--------------------------------------------

case class ResultadoRuleta(numeroResultado: Int) extends ResultadoDeJuego {
  require(numeroResultado >= 0 && numeroResultado < 37)
}

sealed trait JugadaRuleta extends (ResultadoRuleta => Ganancia) with JugadaSimple {
  def ganarTriple(): Ganancia = Ganancia(montoApostado * 3)
  def ganarAlNumero(): Ganancia = Ganancia(montoApostado * 36)
  def distribucionDeGanancias(): DistribucionProbabilidad = {
    GeneradorDistribuciones().distribucionPonderada(List(sucesoGanar, sucesoPerder))
  }
  val sucesoGanar: SucesoPonderado
  val sucesoPerder: SucesoPonderado
}

case class JugarAlRojo(montoApostado: Int) extends JugadaRuleta {
  override def apply(resultado: ResultadoRuleta): Ganancia = resultado match {
    case ResultadoRuleta(numeroResultado) if Rojo.contiene(numeroResultado) => ganarDoble()
    case _ => perder()
  }
  val sucesoGanar: SucesoPonderado = SucesoPonderado(ganarDoble(), Rojo.valores.length)
  val sucesoPerder: SucesoPonderado = SucesoPonderado(perder(), Negro.valores.length + 1)
}

case class JugarAlNegro(montoApostado: Int) extends JugadaRuleta {
  override def apply(resultado: ResultadoRuleta): Ganancia = resultado match {
    case ResultadoRuleta(numeroResultado) if Negro.contiene(numeroResultado) => ganarDoble()
    case _ => perder()
  }
  val sucesoGanar: SucesoPonderado = SucesoPonderado(ganarDoble(), Negro.valores.length)
  val sucesoPerder: SucesoPonderado = SucesoPonderado(perder(), Rojo.valores.length + 1)
}

case class JugarAlNumero(montoApostado: Int, numeroApostado: Int) extends JugadaRuleta {
  override def apply(resultado: ResultadoRuleta): Ganancia = resultado match {
    case ResultadoRuleta(resultado) if resultado == numeroApostado => ganarAlNumero()
    case _ => perder()
  }
  val sucesoGanar: SucesoPonderado = SucesoPonderado(ganarAlNumero(), 1)
  val sucesoPerder: SucesoPonderado = SucesoPonderado(perder(), 36)
}

case class JugarAPar(montoApostado: Int) extends JugadaRuleta {
  override def apply(resultado: ResultadoRuleta): Ganancia = resultado match {
    case ResultadoRuleta(0) => perder()
    case ResultadoRuleta(resultado) if resultado % 2 == 0 => ganarDoble()
    case _ => perder()
  }
  val sucesoGanar: SucesoPonderado = SucesoPonderado(ganarDoble(), 36/2)
  val sucesoPerder: SucesoPonderado = SucesoPonderado(perder(), 36/2 + 1)
}

case class JugarAImpar(montoApostado: Int) extends JugadaRuleta {
  override def apply(resultado: ResultadoRuleta): Ganancia = resultado match {
    case ResultadoRuleta(resultado) if resultado % 2 != 0 => ganarDoble()
    case _ => perder()
  }
  val sucesoGanar: SucesoPonderado = SucesoPonderado(ganarDoble(), 36/2)
  val sucesoPerder: SucesoPonderado = SucesoPonderado(perder(), 36/2 + 1)
}

case class JugarADocena(montoApostado: Int, docenaElegida: Docena) extends JugadaRuleta {
  override def apply(resultado: ResultadoRuleta): Ganancia = resultado match {
    case ResultadoRuleta(resultado) if docenaElegida.contiene(resultado) => ganarTriple()
    case _ => perder()
  }
  val sucesoGanar: SucesoPonderado = SucesoPonderado(ganarTriple(), 1)
  val sucesoPerder: SucesoPonderado = SucesoPonderado(perder(), 2)
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

sealed trait Docena { def contiene(num: Int): Boolean }
case object PrimerDocena extends Docena {
  override def contiene(num: Int): Boolean = num >= 1 && num <= 12
}
case object SegundaDocena extends Docena {
  override def contiene(num: Int): Boolean = num >= 13 && num <= 24
}
case object TercerDocena extends Docena {
  override def contiene(num: Int): Boolean = num >= 25 && num <= 36
}




// 2 - Permitir crear apuestas compuestas para los juegos cuyos resultados se modelaron en el punto anterior.

// APUESTA COMBINADA <<------------------------------
trait ApuestaCombinada extends Jugada
case class CaraCruzCombinada(jugadas: List[JugadaCaraCruz]) extends (ResultadoCaraCruz => Ganancia) with ApuestaCombinada{
  override def apply(resultado: ResultadoCaraCruz): Ganancia = jugadas.foldLeft(Ganancia(0)){
    (ganancia, jugada) => ganancia + jugada(resultado)
  }
}
case class RuletaCombinada(jugadas: List[JugadaRuleta]) extends (ResultadoRuleta => Ganancia) with ApuestaCombinada{
  override def apply(resultado: ResultadoRuleta): Ganancia = jugadas.foldLeft(Ganancia(0)){
    (ganancia, jugada) => ganancia + jugada(resultado)
  }
}



// JUEGOS SUCESIVOS <<------------------------------
case class SucecionDeJugadas(montoInicial: Int, jugadas: List[Jugada])