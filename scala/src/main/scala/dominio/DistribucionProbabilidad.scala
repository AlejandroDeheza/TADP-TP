package dominio

import util.Utils.{Plata, ResultadoRuleta}


// SUCESOS <------------------------------------
case object SucesosCaraCruz { lazy val sucesos: List[ResultadoCaraCruz] = List(Cara, Cruz) }
case object SucesosRuleta   { lazy val sucesos: List[ResultadoRuleta] = (0 to 36).toList }

sealed trait SucesoProb[T] {
  val suceso: T
  val probabilidad: Double
}
case class SucesoConProbabilidad[T](suceso: T, probabilidad: Double) extends SucesoProb[T]
case class SucesoConEstados(suceso: Plata, probabilidad: Double, historial: List[EstadoApuesta]) extends SucesoProb[Plata]

case class SucesoPonderado[T](suceso: T, pesoPonderado: Int)

// DISTRIBUCIONES <<---------------------------------------------
sealed trait sucesosATransformar[T]
case class EventoSeguro[T](suceso: T) extends sucesosATransformar[T]
case class Equiprobables[T](sucesos: List[T]) extends sucesosATransformar[T]
case class Ponderados[T](sucesos: List[SucesoPonderado[T]]) extends sucesosATransformar[T]

case object GeneradorDistribuciones {
   def apply[T](tipoSuceso: sucesosATransformar[T]): DistribucionProbabilidad[T] = {
    val sucesosNuevos: List[SucesoConProbabilidad[T]] = tipoSuceso match {
      case EventoSeguro(suceso)   => List(SucesoConProbabilidad(suceso, 1.0))
      case Equiprobables(sucesos) => for (s <- sucesos) yield SucesoConProbabilidad(s, 1.0 / sucesos.length)
      case Ponderados(sucesos)    => for (s <- sucesos) yield pasarASucesoProbable(s, sucesos)
    }
    DistribucionProbabilidad(sucesosNuevos)
  }

  private def pasarASucesoProbable[T](s: SucesoPonderado[T], list: List[SucesoPonderado[_]]): SucesoConProbabilidad[T] =
    SucesoConProbabilidad(s.suceso, (1.0 / pesoTotal(list)) * s.pesoPonderado)

  private def pesoTotal(sucesos: List[SucesoPonderado[_]]): Int = sucesos.map(_.pesoPonderado).sum
}

sealed trait Distribucion[T] {
  val distribucion: List[SucesoProb[T]]

  lazy val sucesosPosibles: List[T] = for (s <- distribucion if s.probabilidad > 0.0) yield s.suceso

  def probabilidadDe(suceso: T): Double = distribucion.find(s => s.suceso == suceso) match {
    case Some(s) => s.probabilidad
    case _ => 0.0
  }
}

case class DistribucionProbabilidad[T](distribucion: List[SucesoConProbabilidad[T]]) extends Distribucion[T]

case class DistribucionJugadas(distribucion: List[SucesoConEstados]) extends Distribucion[Plata]