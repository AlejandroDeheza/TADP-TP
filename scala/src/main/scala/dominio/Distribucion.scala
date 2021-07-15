package dominio

import util.Utils.Plata


// SUCESOS <------------------------------------
sealed trait SucesoProb[T] {
  val valor: T
  val probabilidad: Double
}
case class SucesoConProbabilidad[T](valor: T, probabilidad: Double) extends SucesoProb[T]

case class SucesoConEstados(valor: Plata, probabilidad: Double, historial: List[EstadoApuesta]) extends SucesoProb[Plata] {

  def indicarQueSiJugo(s: SucesoConProbabilidad[Plata], apuesta: ApuestaSimple[_]): SucesoConEstados = {
    val ganancia = s.valor
    val estado = if (ganancia == 0) Perdio(apuesta) else Gano(apuesta)
    copy(
      valor = valor - apuesta.montoApostado + ganancia,
      probabilidad = probabilidad * s.probabilidad,
      historial = historial ++ List(estado)
    )
  }

  def indicarQueNoJugo(apuesta: ApuestaSimple[_]): SucesoConEstados = copy(historial = historial ++ List(NoJugo(apuesta)))
}

case class SucesoPonderado[T](valor: T, pesoPonderado: Int)



// DISTRIBUCIONES <<---------------------------------------------
sealed trait SucesosATransformar[T]
case class EventoSeguro[T](valor: T) extends SucesosATransformar[T]
case class Equiprobables[T](valores: List[T]) extends SucesosATransformar[T]
case class Ponderados[T](valores: List[SucesoPonderado[T]]) extends SucesosATransformar[T]

case object GeneradorDistribuciones {
  def apply[T](tipoSuceso: SucesosATransformar[T]): DistribucionProbabilidad[T] = {
    val sucesosNuevos = tipoSuceso match {
      case EventoSeguro(valor)    => List(SucesoConProbabilidad(valor, 1.0))
      case Equiprobables(valores) => for (v <- valores) yield SucesoConProbabilidad(v, 1.0 / valores.length)
      case Ponderados(valores)    => for (v <- valores) yield pasarASucesoProbable(v, valores)
    }
    DistribucionProbabilidad(sucesosNuevos)
  }

  private def pasarASucesoProbable[T](s: SucesoPonderado[T], list: List[SucesoPonderado[_]]): SucesoConProbabilidad[T] =
    SucesoConProbabilidad(s.valor, (1.0 / pesoTotal(list)) * s.pesoPonderado)

  private def pesoTotal(sucesos: List[SucesoPonderado[_]]): Int = sucesos.map(_.pesoPonderado).sum
}

sealed trait Distribucion[T] {
  val sucesos: List[SucesoProb[T]]

  lazy val sucesosPosibles: List[T] = for (s <- sucesos if s.probabilidad > 0.0) yield s.valor

  def probabilidadDe(valor: T): Double = sucesos.find(s => s.valor == valor) match {
    case Some(s) => s.probabilidad
    case _ => 0.0
  }
}

case class DistribucionProbabilidad[T](sucesos: List[SucesoConProbabilidad[T]]) extends Distribucion[T]

case class DistribucionApuestas(sucesos: List[SucesoConEstados], montoInicial: Plata) extends Distribucion[Plata]