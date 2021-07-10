package Ale

sealed trait SucesoGenerico

sealed trait ResultadoDeJuego extends SucesoGenerico

case class Ganancia(monto: Int) extends SucesoGenerico {
  def +(g: Ganancia): Ganancia = Ganancia(g.monto + monto)
}

sealed trait Juego

sealed trait Jugada

sealed trait Apuesta

sealed trait ApuestaSimple extends Apuesta {
  val montoApostado: Int
  def perder(): Ganancia = Ganancia(0)
  def ganarDoble(): Ganancia = Ganancia(montoApostado * 2)
  def distribucionDeGanancias(): DistribucionProbabilidad
}




// 1 - Modelar los resultados de juegos posibles
// para la ruleta y para ‘cara o cruz’
// y las jugadas mencionadas que se pueden hacer sobre ellos.

sealed trait ResultadoCaraCruz extends ResultadoDeJuego
case object Cara extends ResultadoCaraCruz
case object Cruz extends ResultadoCaraCruz

sealed trait ApuestaCaraCruz extends (ResultadoCaraCruz => Ganancia) with ApuestaSimple {
  def distribucionDeGanancias(): DistribucionProbabilidad = {
    GeneradorDistribuciones().distribucionEquiprobable(List(perder(), ganarDoble()))
  }
}
case class CaraCruz(jugada: JugadaCaraCruz) extends Juego {
  def apply(montoApostado: Int): ApuestaCaraCruz =
    if (jugada == ACara()) DuplicarSiSaleCara(montoApostado) else DuplicarSiSaleCruz(montoApostado)
}
sealed trait JugadaCaraCruz extends Jugada
case class ACara() extends JugadaCaraCruz
case class ACruz() extends JugadaCaraCruz

case class DuplicarSiSaleCara(montoApostado: Int) extends ApuestaCaraCruz {
  override def apply(resultado: ResultadoCaraCruz): Ganancia = resultado match {
    case Cara => ganarDoble()
    case _ => perder()
  }
}

case class DuplicarSiSaleCruz(montoApostado: Int) extends ApuestaCaraCruz {
  override def apply(resultado: ResultadoCaraCruz): Ganancia = resultado match {
    case Cruz => ganarDoble()
    case _ => perder()
  }
}



// RULETA <--------------------------------------------

case class ResultadoRuleta(numeroResultado: Int) extends ResultadoDeJuego {
  require(numeroResultado >= 0 && numeroResultado < 37)
}

sealed trait ApuestaRuleta extends (ResultadoRuleta => Ganancia) with ApuestaSimple {
  def ganarTriple(): Ganancia = Ganancia(montoApostado * 3)
  def ganarAlNumero(): Ganancia = Ganancia(montoApostado * 36)
  def distribucionDeGanancias(): DistribucionProbabilidad = {
    GeneradorDistribuciones().distribucionPonderada(List(sucesoGanar, sucesoPerder))
  }
  val sucesoGanar: SucesoPonderado
  val sucesoPerder: SucesoPonderado
}

case class Ruleta(jugada: JugadaRuleta) extends Juego {
  def apply(montoApostado: Int): ApuestaRuleta = jugada match {
    case AlRojo() => JugarAlRojo(montoApostado)
    case AlNegro() => JugarAlNegro(montoApostado)
    case APar() => JugarAPar(montoApostado)
    case AImpar() => JugarAImpar(montoApostado)
    case AlNumero(numeroApostado) => JugarAlNumero(montoApostado, numeroApostado)
    case ADocena(docenaElegida) => JugarADocena(montoApostado, docenaElegida)
  }
}
sealed trait JugadaRuleta extends Jugada
case class AlRojo() extends JugadaRuleta
case class AlNegro() extends JugadaRuleta
case class APar() extends JugadaRuleta
case class AImpar() extends JugadaRuleta
case class AlNumero(numeroApostado: Int) extends JugadaRuleta
case class ADocena(docenaElegida: Docena) extends JugadaRuleta

case class JugarAlRojo(montoApostado: Int) extends ApuestaRuleta {
  override def apply(resultado: ResultadoRuleta): Ganancia = resultado match {
    case ResultadoRuleta(numeroResultado) if Rojo.contiene(numeroResultado) => ganarDoble()
    case _ => perder()
  }
  val sucesoGanar: SucesoPonderado = SucesoPonderado(ganarDoble(), Rojo.valores.length)
  val sucesoPerder: SucesoPonderado = SucesoPonderado(perder(), Negro.valores.length + 1)
}

case class JugarAlNegro(montoApostado: Int) extends ApuestaRuleta {
  override def apply(resultado: ResultadoRuleta): Ganancia = resultado match {
    case ResultadoRuleta(numeroResultado) if Negro.contiene(numeroResultado) => ganarDoble()
    case _ => perder()
  }
  val sucesoGanar: SucesoPonderado = SucesoPonderado(ganarDoble(), Negro.valores.length)
  val sucesoPerder: SucesoPonderado = SucesoPonderado(perder(), Rojo.valores.length + 1)
}

case class JugarAlNumero(montoApostado: Int, numeroApostado: Int) extends ApuestaRuleta {
  override def apply(resultado: ResultadoRuleta): Ganancia = resultado match {
    case ResultadoRuleta(resultado) if resultado == numeroApostado => ganarAlNumero()
    case _ => perder()
  }
  val sucesoGanar: SucesoPonderado = SucesoPonderado(ganarAlNumero(), 1)
  val sucesoPerder: SucesoPonderado = SucesoPonderado(perder(), 36)
}

case class JugarAPar(montoApostado: Int) extends ApuestaRuleta {
  override def apply(resultado: ResultadoRuleta): Ganancia = resultado match {
    case ResultadoRuleta(0) => perder()
    case ResultadoRuleta(resultado) if resultado % 2 == 0 => ganarDoble()
    case _ => perder()
  }
  val sucesoGanar: SucesoPonderado = SucesoPonderado(ganarDoble(), 36/2)
  val sucesoPerder: SucesoPonderado = SucesoPonderado(perder(), 36/2 + 1)
}

case class JugarAImpar(montoApostado: Int) extends ApuestaRuleta {
  override def apply(resultado: ResultadoRuleta): Ganancia = resultado match {
    case ResultadoRuleta(resultado) if resultado % 2 != 0 => ganarDoble()
    case _ => perder()
  }
  val sucesoGanar: SucesoPonderado = SucesoPonderado(ganarDoble(), 36/2)
  val sucesoPerder: SucesoPonderado = SucesoPonderado(perder(), 36/2 + 1)
}

case class JugarADocena(montoApostado: Int, docenaElegida: Docena) extends ApuestaRuleta {
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
trait ApuestaCombinada extends Apuesta {
  val apuestas: List[ApuestaSimple]
  def montoTotalApostado(): Int = apuestas.map(_.montoApostado).sum
  def distribucionesDeGanancias(): List[DistribucionProbabilidad] = { apuestas.map(_.distribucionDeGanancias()) }
  def distribucionesConMontos(): List[DistribucionConMontos] = {
    apuestas.map(d => DistribucionConMontos(d.distribucionDeGanancias(), d.montoApostado))
  }
}

case class DistribucionConMontos(distribucion: DistribucionProbabilidad, montoApostado: Int)

case class CaraCruzCombinada(apuestas: List[ApuestaCaraCruz]) extends (ResultadoCaraCruz => Ganancia) with ApuestaCombinada {
  override def apply(resultado: ResultadoCaraCruz): Ganancia = apuestas.foldLeft(Ganancia(0)){
    (ganancia, jugada) => ganancia + jugada(resultado)
  }
}
case class RuletaCombinada(apuestas: List[ApuestaRuleta]) extends (ResultadoRuleta => Ganancia) with ApuestaCombinada {
  override def apply(resultado: ResultadoRuleta): Ganancia = apuestas.foldLeft(Ganancia(0)){
    (ganancia, jugada) => ganancia + jugada(resultado)
  }
}



// JUEGOS SUCESIVOS <<------------------------------
case class JuegosSucesivos(apuestas: List[Apuesta]) {

  def apply(montoInicial: Int): DistribucionProbabilidad = {
    apuestas.foldLeft(DistribucionProbabilidad(List(SucesoConProbabilidad(Ganancia(montoInicial), 1.0)))) {
      (distribucion, apuesta) => apuesta match {
          case simple: ApuestaSimple =>
            distribucion.agregarSucesos(simple.distribucionDeGanancias(), simple.montoApostado)
          case combinada: ApuestaCombinada => DistribucionProbabilidad(List())
            distribucion.agregarSucesos(combinada.distribucionesConMontos())
        }
    }
  }
}