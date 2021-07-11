package Ale

import Ale.Utils.{Plata, ResultadoRuleta}


// SUCESOS <------------------------------------
case object SucesosCaraCruz {
  val sucesos: List[ResultadoCaraCruz] = List(Cara, Cruz)
}
case object SucesosRuleta {
  val sucesos: List[ResultadoRuleta] = (0 to 36).toList
}

case class SucesoPonderado[T](suceso: T, pesoPonderado: Int) {
  def pasarASucesoProbable(pesoTotal: Int): SucesoConProbabilidad[T] = {
    SucesoConProbabilidad(suceso, (1.0 / pesoTotal) * pesoPonderado)
  }
}

case class SucesoConProbabilidad[T](suceso: T, probabilidad: Double)


// DISTRIBUCIONES <<---------------------------------------------
class GeneradorDistribuciones[T] {
  def eventoSeguro(suceso: T): DistribucionProbabilidad[T] = {
    DistribucionProbabilidad(List(SucesoConProbabilidad(suceso, 1.0)))
  }

  def Equiprobable(sucesos: List[T]): DistribucionProbabilidad[T] = {
    val cantidad: Int = sucesos.length
    DistribucionProbabilidad(sucesos.map(suceso => SucesoConProbabilidad(suceso, 1.0 / cantidad)))
  }

  def Ponderado(sucesos: List[SucesoPonderado[T]]): DistribucionProbabilidad[T] = {
    val pesoTotal: Int = sucesos.map(_.pesoPonderado).sum
    DistribucionProbabilidad(sucesos.map(_.pasarASucesoProbable(pesoTotal)))
  }
}

case class DistribucionGananciasConMonto(distribucion: DistribucionProbabilidad[Plata], montoApostado: Plata)

// hay alguna forma de decir ---> T = ResultadoCaraCruz || Int // TODO: REVISAR
case class DistribucionProbabilidad[T](distribucion: List[SucesoConProbabilidad[T]]) {

  def sucesosPosibles(): List[T] = distribucion.filter(_.probabilidad > 0.0).map(_.suceso)

  def probabilidadDe(suceso: T): Double = {
    distribucion.find(su => su.suceso == suceso) match {
      case Some(resultado) => resultado.probabilidad
      case _ => 0.0
    }
  }
}
