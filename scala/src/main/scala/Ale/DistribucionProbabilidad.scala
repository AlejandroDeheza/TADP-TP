package Ale

import Ale.Utils.{Plata, head, last}

case object SucesosCaraCruz {
  val sucesos: List[ResultadoCaraCruz] = List(Cara, Cruz)
}
case object SucesosRuleta {
  val sucesos: List[ResultadoRuleta] = (0 to 36).map(i => ResultadoRuleta(i)).toList
}

case class SucesoPonderado(resultadoDeJuego: SucesoGenerico, pesoPonderado: Int) {
  def pasarASucesoProbable(pesoTotal: Int): SucesoConProbabilidad = {
    SucesoConProbabilidad(resultadoDeJuego, (1.0 / pesoTotal) * pesoPonderado)
  }
}

case class SucesoConProbabilidad(sucesoGenerico: SucesoGenerico, probabilidad: Double) {
  def monto: Plata = sucesoGenerico.asInstanceOf[PlataWrapper].monto //TODO: revisar
}

/*case class GeneradorDistribuciones[T <: DistribucionProbabilidad]() {
  def eventoSeguro(suceso: SucesoGenerico): T = {
    T(List(SucesoConProbabilidad(suceso, 1.0)))
  }

  def Equiprobable(sucesos: List[SucesoGenerico]): T = {
    val cantidad: Int = sucesos.length
    T(sucesos.map(suceso => SucesoConProbabilidad(suceso, 1.0 / cantidad)))
  }

  def Ponderada(sucesos: List[SucesoPonderado]): T = {
    val pesoTotal: Int = sucesos.map(_.pesoPonderado).sum
    T(sucesos.map(_.pasarASucesoProbable(pesoTotal)))
  }
}*/   // TODO: ARREGLAR

case class GeneradorDistribucionesResultados() {
  def eventoSeguro(suceso: SucesoGenerico): DistribucionResultados = {
    DistribucionResultados(List(SucesoConProbabilidad(suceso, 1.0)))
  }

  def Equiprobable(sucesos: List[SucesoGenerico]): DistribucionResultados = {
    val cantidad: Int = sucesos.length
    DistribucionResultados(sucesos.map(suceso => SucesoConProbabilidad(suceso, 1.0 / cantidad)))
  }

  def Ponderado(sucesos: List[SucesoPonderado]): DistribucionResultados = {
    val pesoTotal: Int = sucesos.map(_.pesoPonderado).sum
    DistribucionResultados(sucesos.map(_.pasarASucesoProbable(pesoTotal)))
  }
} // TODO: ARREGLAR

case class GeneradorDistribucionesGanancias() {
  def eventoSeguro(suceso: SucesoGenerico): DistribucionGanancias = {
    DistribucionGanancias(List(SucesoConProbabilidad(suceso, 1.0)))
  }

  def Equiprobable(sucesos: List[SucesoGenerico]): DistribucionGanancias = {
    val cantidad: Int = sucesos.length
    DistribucionGanancias(sucesos.map(suceso => SucesoConProbabilidad(suceso, 1.0 / cantidad)))
  }

  def Ponderado(sucesos: List[SucesoPonderado]): DistribucionGanancias = {
    val pesoTotal: Int = sucesos.map(_.pesoPonderado).sum
    DistribucionGanancias(sucesos.map(_.pasarASucesoProbable(pesoTotal)))
  }
} // TODO: ARREGLAR


sealed trait DistribucionProbabilidad {
  val distribucion: List[SucesoConProbabilidad]

  def sucesosPosibles(): List[SucesoGenerico] = distribucion.filter(_.probabilidad > 0.0).map(_.sucesoGenerico)

  def probabilidadDe(suceso: SucesoGenerico): Double = {
    distribucion.find(su => su.sucesoGenerico == suceso) match {
      case Some(resultado) => resultado.probabilidad
      case _ => 0.0
    }
  }
}

case class DistribucionGananciasConMonto(distribucion: DistribucionGanancias, montoApostado: Plata)

case class DistribucionResultados(distribucion: List[SucesoConProbabilidad]) extends DistribucionProbabilidad

case class DistribucionGanancias(distribucion: List[SucesoConProbabilidad]) extends DistribucionProbabilidad {

  def agregarSuceso(distribDe2Sucesos: DistribucionGanancias, montoApostado: Plata): DistribucionGanancias = {
    val sucesosNormales = distribucion.filter(s => s.monto >= montoApostado)
    val sucesosConPocaPlata = distribucion.filter(s => s.monto < montoApostado)

    val sucesosNuevos = sucesosNormales.map(s => generarSuceso(s, montoApostado, distribDe2Sucesos, head))
    val sucesosNuevos2 = sucesosNormales.map(s => generarSuceso(s, montoApostado, distribDe2Sucesos, last))

    DistribucionGanancias(sucesosNuevos ++ sucesosNuevos2 ++ sucesosConPocaPlata)
  }

  def agregarSucesos(distribucionesConMontos: List[DistribucionGananciasConMonto]): DistribucionGanancias = {
    distribucionesConMontos.foldLeft(copy()) { (distrib, distribConMontos) =>
      distrib.agregarSuceso(distribConMontos.distribucion, distribConMontos.montoApostado)
    }
  }

  private def generarSuceso(s: SucesoConProbabilidad, montoApostado: Plata, d: DistribucionGanancias,
                            f: Iterable[SucesoConProbabilidad] => SucesoConProbabilidad): SucesoConProbabilidad = {
    val montoInicial = s.monto
    val ganancia = f(d.copy().distribucion).monto
    val multiplicacionProbabilidades = s.probabilidad * f(d.copy().distribucion).probabilidad

    SucesoConProbabilidad( PlataWrapper(montoInicial - montoApostado + ganancia), multiplicacionProbabilidades )
  }
}
