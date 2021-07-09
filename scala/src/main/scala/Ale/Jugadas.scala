package Ale

sealed trait ResultadoDeJuego

case class Ganancia(monto: Int) {
  def +(g: Ganancia): Ganancia = Ganancia(g.monto + monto)
}

sealed trait Jugada extends {
  val montoApostado: Int
} // tomo a "jugada" y "apuesta" como un mismo objeto --> Jugada




// 1 - Modelar los resultados de juegos posibles
// para la ruleta y para ‘cara o cruz’
// y las jugadas mencionadas que se pueden hacer sobre ellos.

sealed trait ResultadoCaraCruz extends ResultadoDeJuego
case object Cara extends ResultadoCaraCruz
case object Cruz extends ResultadoCaraCruz

sealed trait JugadaCaraCruz extends (ResultadoCaraCruz => Ganancia) with Jugada

case class DuplicarSiSaleCara(montoApostado: Int) extends JugadaCaraCruz {
  override def apply(resultado: ResultadoCaraCruz): Ganancia = resultado match {
    case Cara => Ganancia(montoApostado * 2)
    case _ => Ganancia(0)
  }
}

case class DuplicarSiSaleCruz(montoApostado: Int) extends JugadaCaraCruz {
  override def apply(resultado: ResultadoCaraCruz): Ganancia = resultado match {
    case Cruz => Ganancia(montoApostado * 2)
    case _ => Ganancia(0)
  }
}



// RULETA <--------------------------------------------

case class ResultadoRuleta(numeroResultado: Int) extends ResultadoDeJuego {
  require(numeroResultado >= 0 && numeroResultado < 37)
}

sealed trait JugadaRuleta extends (ResultadoRuleta => Ganancia) with Jugada

case class JugarAlRojo(montoApostado: Int) extends JugadaRuleta {
  override def apply(resultado: ResultadoRuleta): Ganancia = resultado match {
    case ResultadoRuleta(numeroResultado) if Rojo.contiene(numeroResultado) => Ganancia(montoApostado * 2)
    case _ => Ganancia(0)
  }
}

case class JugarAlNegro(montoApostado: Int) extends JugadaRuleta {
  override def apply(resultado: ResultadoRuleta): Ganancia = resultado match {
    case ResultadoRuleta(numeroResultado) if Negro.contiene(numeroResultado) => Ganancia(montoApostado * 2)
    case _ => Ganancia(0)
  }
}

case class JugarAlNumero(montoApostado: Int, numeroApostado: Int) extends JugadaRuleta {
  override def apply(resultado: ResultadoRuleta): Ganancia = resultado match {
    case ResultadoRuleta(resultado) if resultado == numeroApostado => Ganancia(montoApostado * 36)
    case _ => Ganancia(0)
  }
}

case class JugarAPar(montoApostado: Int) extends JugadaRuleta {
  override def apply(resultado: ResultadoRuleta): Ganancia = resultado match {
    case ResultadoRuleta(0) => Ganancia(0)
    case ResultadoRuleta(resultado) if resultado % 2 == 0 => Ganancia(montoApostado * 2)
    case _ => Ganancia(0)
  }
}

case class JugarAImpar(montoApostado: Int) extends JugadaRuleta {
  override def apply(resultado: ResultadoRuleta): Ganancia = resultado match {
    case ResultadoRuleta(resultado) if resultado % 2 != 0 => Ganancia(montoApostado * 2)
    case _ => Ganancia(0)
  }
}

case class JugarADocena(montoApostado: Int, docenaElegida: Docena) extends JugadaRuleta {
  override def apply(resultado: ResultadoRuleta): Ganancia = resultado match {
    case ResultadoRuleta(resultado) if docenaElegida.contiene(resultado) => Ganancia(montoApostado * 3)
    case _ => Ganancia(0)
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

// JUGADA COMPUESTA <<------------------------------
case class JugadaCompuesta(jugadas: List[JugadaRuleta]) extends (ResultadoRuleta => Ganancia) {
  override def apply(resultado: ResultadoRuleta): Ganancia = jugadas.foldLeft(Ganancia(0)){
    (ganancia, jugada) => ganancia + jugada(resultado)
  }
}

