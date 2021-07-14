package dominio

import util.Utils.{Plata, ResultadoRuleta}


// SUCESOS <------------------------------------
case object SucesosCaraCruz { lazy val sucesos: List[ResultadoCaraCruz] = List(Cara, Cruz) }
case object SucesosRuleta   { lazy val sucesos: List[ResultadoRuleta] = (0 to 36).toList }

sealed trait SucesoRelevante[T] {
  val suceso: T
  val probabilidad: Double
}
case class SucesoConProbabilidad[T](suceso: T, probabilidad: Double) extends SucesoRelevante[T]
case class SucesoConEstados(suceso: Plata, probabilidad: Double, historialDeEstados: List[EstadoApuesta]) extends SucesoRelevante[Plata]

case class SucesoPonderado[T](suceso: T, pesoPonderado: Int) {

  def pasarASucesoProbable(pesoTotal: Int): SucesoConProbabilidad[T] = {
    SucesoConProbabilidad(suceso, (1.0 / pesoTotal) * pesoPonderado)
  }
}

// DISTRIBUCIONES <<---------------------------------------------
class GeneradorDistribuciones[T] {
  def eventoSeguro(suceso: T): DistribucionProbabilidad[T] = {
    DistribucionProbabilidad(List(SucesoConProbabilidad(suceso, 1.0)))
  }

  def equiprobable(sucesos: List[T]): DistribucionProbabilidad[T] = {
    DistribucionProbabilidad(
      for (s <- sucesos) yield SucesoConProbabilidad(s, 1.0 / sucesos.length)
    )
  }

  def ponderado(sucesos: List[SucesoPonderado[T]]): DistribucionProbabilidad[T] = {
    val pesoTotal: Int = sucesos.map(_.pesoPonderado).sum
    DistribucionProbabilidad(
      for (s <- sucesos) yield s.pasarASucesoProbable(pesoTotal)
    )
  }
}

sealed trait Distribucion[T] {
  val distribucion: List[SucesoRelevante[T]]

  lazy val sucesosPosibles: List[T] = for (s <- distribucion if s.probabilidad > 0.0) yield s.suceso

  def probabilidadDe(suceso: T): Double = distribucion.find(s => s.suceso == suceso) match {
    case Some(s) => s.probabilidad
    case _ => 0.0
  }
}

case class DistribucionProbabilidad[T](distribucion: List[SucesoConProbabilidad[T]]) extends Distribucion[T]

case class DistribucionJugadas(distribucion: List[SucesoConEstados]) extends Distribucion[Plata]