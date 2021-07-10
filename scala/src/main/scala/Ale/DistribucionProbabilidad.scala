package Ale

case object SucesosCaraCruz {
  val sucesos: List[ResultadoCaraCruz] = List(Cara, Cruz)
}
case object SucesosRuleta {
  val sucesos: List[ResultadoRuleta] = (0 to 36).map(i => ResultadoRuleta(i)).toList
}


case class GeneradorDistribuciones() {
  def eventoSeguro(suceso: SucesoGenerico): DistribucionProbabilidad = {
    DistribucionProbabilidad(List(SucesoConProbabilidad(suceso, 100.0)))
  }
  def distribucionEquiprobable(sucesos: List[SucesoGenerico]): DistribucionProbabilidad = {
    val cantidad: Int = sucesos.length
    DistribucionProbabilidad(sucesos.map(suceso => SucesoConProbabilidad(suceso, 100.0 / cantidad)))
  }
  def distribucionPonderada(sucesos: List[SucesoPonderado]): DistribucionProbabilidad = {
    val pesoTotal: Int = sucesos.map(_.pesoPonderado).sum
    DistribucionProbabilidad(sucesos.map(_.pasarASucesoProbable(pesoTotal)))
  }
}


case class SucesoPonderado(resultadoDeJuego: SucesoGenerico, pesoPonderado: Int) {
  def pasarASucesoProbable(pesoTotal: Int): SucesoConProbabilidad = {
    SucesoConProbabilidad(resultadoDeJuego, (100.0 / pesoTotal) * pesoPonderado)
  }
}

case class SucesoConProbabilidad(resultadoDeJuego: SucesoGenerico, probabilidad: Double)

case class DistribucionProbabilidad(distribucion: List[SucesoConProbabilidad]) {
  def sucesosPosibles() : List[SucesoGenerico] = distribucion.filter(_.probabilidad > 0.0).map(_.resultadoDeJuego)

  def probabilidadDe(suceso: SucesoGenerico) :Double = {
    distribucion.find(su => su.resultadoDeJuego == suceso) match {
      case Some(resultado) => resultado.probabilidad
      case _ => 0.0
    }
  }
}
