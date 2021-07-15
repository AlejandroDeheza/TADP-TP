package dominio

sealed trait ModalidadRuleta {
  val mult: Int
  val multSiPierde: Int = 0
  val probGanarYPerder: (Double, Double)
}

sealed trait ModalidadBasica extends ModalidadRuleta { lazy val probGanarYPerder: (Double, Double) = (18/37.0, 19/37.0) }

sealed trait Docena extends ModalidadRuleta {
  lazy val probGanarYPerder: (Double, Double) = (12/37.0, 25/37.0)
  val rango: Range
  def contiene(num: Int): Boolean = rango.contains(num)
}

// CASE CLASS para cuando se desea cambiar el multiplicador <<------------------------------
case class Rojo         (mult: Int = 2) extends ModalidadBasica
case class Negro        (mult: Int = 2) extends ModalidadBasica
case class Par          (mult: Int = 2) extends ModalidadBasica
case class Impar        (mult: Int = 2) extends ModalidadBasica

case class PrimerDocena (mult: Int = 3) extends Docena { lazy val rango: Range = 1 to 12 }
case class SegundaDocena(mult: Int = 3) extends Docena { lazy val rango: Range = 13 to 24 }
case class TercerDocena (mult: Int = 3) extends Docena { lazy val rango: Range = 25 to 36 }

case class Al(numeroElegido: Int, mult: Int = 36) extends ModalidadRuleta {
  lazy val probGanarYPerder: (Double, Double) = (1/37.0, 36/37.0)
}



// COMPANION OBJECTS para cuando no hace falta modificar nada <<------------------------------
sealed trait Color extends ModalidadBasica {
  lazy val mult: Int = 2
  val valores: List[Int]
  def contiene(num: Int): Boolean = valores.contains(num)
} // "Color" originalmente estaba solo para tener los valores posibles de un color
case object Rojo extends Color {
  lazy val valores: List[Int] = List(1, 3, 5, 7,  9, 12, 14, 16, 18, 19, 21, 23, 25, 27, 30, 32, 34, 36)
}
case object Negro extends Color {
  lazy val valores: List[Int] = List(2, 4, 6, 8, 10, 11, 13, 15, 17, 20, 22, 24, 26, 28, 29, 31, 33, 35)
}

case object Par extends ModalidadBasica   { lazy val mult: Int = 2 }
case object Impar extends ModalidadBasica { lazy val mult: Int = 2 }

sealed trait DocenaObject extends Docena { lazy val mult: Int = 3 }
case object PrimerDocena extends DocenaObject  { lazy val rango: Range =  1 to 12 }
case object SegundaDocena extends DocenaObject { lazy val rango: Range = 13 to 24 }
case object TercerDocena extends DocenaObject  { lazy val rango: Range = 25 to 36 }
