package Ale

object comparadorDoubles {
  implicit val precision: Precision = Precision(0.0001)

  case class Precision(p:Double)

  implicit class Comparador(val d:Double) extends AnyVal {
    def ~=(d2:Double)(implicit p:Precision): Boolean = (d - d2).abs < p.p
  }
}

case class algo()
