package Ale

case object SucesosCaraCruz {
  val sucesos: List[ResultadoCaraCruz] = List(Cara, Cruz)
}
case object SucesosRuleta {
  val sucesos: List[ResultadoRuleta] = (0 to 36).map(i => ResultadoRuleta(i)).toList
}


case class GeneradorDistribuciones() {
  def eventoSeguro(suceso: SucesoGenerico): DistribucionProbabilidad = {
    DistribucionProbabilidad(List(SucesoConProbabilidad(suceso, 1.0)))
  }
  def distribucionEquiprobable(sucesos: List[SucesoGenerico]): DistribucionProbabilidad = {
    val cantidad: Int = sucesos.length
    DistribucionProbabilidad(sucesos.map(suceso => SucesoConProbabilidad(suceso, 1.0 / cantidad)))
  }
  def distribucionPonderada(sucesos: List[SucesoPonderado]): DistribucionProbabilidad = {
    val pesoTotal: Int = sucesos.map(_.pesoPonderado).sum
    DistribucionProbabilidad(sucesos.map(_.pasarASucesoProbable(pesoTotal)))
  }
}


case class SucesoPonderado(resultadoDeJuego: SucesoGenerico, pesoPonderado: Int) {
  def pasarASucesoProbable(pesoTotal: Int): SucesoConProbabilidad = {
    SucesoConProbabilidad(resultadoDeJuego, (1.0 / pesoTotal) * pesoPonderado)
  }
}

case class SucesoConProbabilidad(sucesoGenerico: SucesoGenerico, probabilidad: Double)

case class DistribucionProbabilidad(distribucion: List[SucesoConProbabilidad]) {
  def sucesosPosibles() : List[SucesoGenerico] = distribucion.filter(_.probabilidad > 0.0).map(_.sucesoGenerico)

  def probabilidadDe(suceso: SucesoGenerico) :Double = {
    distribucion.find(su => su.sucesoGenerico == suceso) match {
      case Some(resultado) => resultado.probabilidad
      case _ => 0.0
    }
  }

  def agregarSucesos(distrib: DistribucionProbabilidad, montoApostado: Int): DistribucionProbabilidad = {
    val unaLista = distribucion.filter(s => s.sucesoGenerico.asInstanceOf[Ganancia].monto >= montoApostado)
    val otraLista = distribucion.filter(s => s.sucesoGenerico.asInstanceOf[Ganancia].monto < montoApostado)

    val lista1 = unaLista.map(suce => SucesoConProbabilidad(
      Ganancia(suce.sucesoGenerico.asInstanceOf[Ganancia].monto - montoApostado + distrib.copy().distribucion.head
        .sucesoGenerico.asInstanceOf[Ganancia].monto),
      suce.probabilidad * distrib.copy().distribucion.head.probabilidad
    ))
    val lista2 = unaLista.map(suce => SucesoConProbabilidad(
      Ganancia(suce.sucesoGenerico.asInstanceOf[Ganancia].monto - montoApostado + distrib.copy().distribucion.last.
        sucesoGenerico.asInstanceOf[Ganancia].monto),
      suce.probabilidad * distrib.copy().distribucion.last.probabilidad
    ))

    DistribucionProbabilidad(lista1 ++ lista2 ++ otraLista)
  }

  def agregarSucesos(distribucionesConMontos: List[DistribucionConMontos]): DistribucionProbabilidad = {
    distribucionesConMontos.foldLeft(copy()){(distrib, distribConMontos) =>
      distrib.agregarSucesos(distribConMontos.distribucion, distribConMontos.montoApostado) }
  }
}
