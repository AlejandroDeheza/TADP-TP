/*package Juego

case class Jugada() extends (Int => Apuesta) //TODO, esta definicion esta bien? (Int => Apuesta)
case class Apuesta(monto: Int) extends (Resultado => Int)
trait Resultado


// CARA CRUZ <<-----------------

trait JugadaCaraCruz extends (Int => ApuestaCaraCruz)
trait ApuestaCaraCruz extends (ResultadoCaraCruz => Int)
trait ResultadoCaraCruz extends Resultado
case object Cara extends ResultadoCaraCruz
case object Cruz extends ResultadoCaraCruz

case class ACara() extends JugadaCaraCruz {
  override def apply(monto: Int): ApuestaCaraCruz = ApuestaCara(monto)
}
case class ACruz() extends JugadaCaraCruz { // TODO es lo mismo que el anterior... no?
  override def apply(monto: Int): ApuestaCaraCruz = ApuestaCruz(monto)
}

case class ApuestaCara(monto: Int) extends ApuestaCaraCruz {
  override def apply(resultado: ResultadoCaraCruz): Int = if (resultado == Cara) monto * 2 else 0
}
case class ApuestaCruz(monto: Int) extends ApuestaCaraCruz {
  override def apply(resultado: ResultadoCaraCruz): Int = if (resultado == Cruz) monto * 2 else 0
}


// RULETA <<-----------------

case class Ruleta(seJugoA: JugadaRuleta, numeroObtenido: Int) extends Juego {
  // TODO, deberia limitar el numero de 0 a 36? como se tira excepcion en constructor?
  override def apply(monto: Int): Int = seJugoA match {
    case AlRojo() => if (Rojo.contiene(numeroObtenido)) monto * 2 else 0
    case AlNegro() => if (Negro.contiene(numeroObtenido)) monto * 2 else 0
    case AlNumero(numeroElegido) => if (numeroElegido == numeroObtenido) monto * 36 else 0
    case APar() => if (numeroObtenido % 2 == 0) monto * 2 else 0 //TODO EXCEPTION, el 0 no cuenta como par ni impar
    case AImpar() => if (numeroObtenido % 2 != 0) monto * 2 else 0 //TODO EXCEPTION
    case ADocena(docenaElegida) => if (esDocena(docenaElegida)) monto * 3 else 0
  }

  private def esDocena(docenaElegida: Int): Boolean = {
    if (docenaElegida == 1 && numeroObtenido >= 1 && numeroObtenido <= 12) return true
    if (docenaElegida == 2 && numeroObtenido >= 13 && numeroObtenido <= 24) return true
    if (docenaElegida == 3 && numeroObtenido >= 25 && numeroObtenido <= 36) return true
    false
  }
}

trait JugadaRuleta extends (Int => ApuestaRuleta)
trait ApuestaRuleta extends (Int => Int)

case class AlRojo() extends JugadaRuleta {
  override def apply(monto: Int): ApuestaRuleta =  ApuestaAlRojo(monto)
}
case class AlNegro() extends JugadaRuleta {
  override def apply(monto: Int): ApuestaRuleta =  ApuestaAlNegro(monto)
}
case class AlNumero(numeroElegido: Int) extends JugadaRuleta
case class APar() extends JugadaRuleta
case class AImpar() extends JugadaRuleta
case class ADocena(docenaElegida: Int) extends JugadaRuleta

case class ApuestaAlRojo(monto: Int) extends ApuestaRuleta {
  override def apply(resultado: Int): Int = if (Rojo.contiene(resultado)) monto * 2 else 0
}
case class ApuestaAlNegro(monto: Int) extends ApuestaRuleta {
  override def apply(resultado: Int): Int = if (Negro.contiene(resultado)) monto * 2 else 0
}


trait Color {
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
case object SinColor extends Color { //TODO, no lo uso, almenos no todavia...
  val valores: List[Int] = List(0)
  override def contiene(num: Int): Boolean = valores.contains(num)
}*/
