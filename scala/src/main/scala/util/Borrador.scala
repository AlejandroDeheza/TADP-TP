package tests

/*def tieneEventoSeguro(): Boolean = distribucion.length == 1

def esEquiprobable(): Boolean = {
  import Ale.comparadorDoubles.Comparador
  val unaProbabilidad = distribucion.head.probabilidad
  distribucion.forall(_.probabilidad ~= unaProbabilidad)
}
}*/




/*
extends (TipoSuceso[_] => DistribucionProbabilidad[_])
* */




/*    case lista: List[SucesoPonderado[T]] =>
      case lista: List[T] =>
      case algo: T =>
*/




/*class GeneradorDistribuciones[T] {
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
      for (s <- sucesos) yield pasarASucesoProbable(s, pesoTotal)
    )
  }

  private def pasarASucesoProbable(s: SucesoPonderado[T], pesoTotal: Int): SucesoConProbabilidad[T] = {
    SucesoConProbabilidad(s.suceso, (1.0 / pesoTotal) * s.pesoPonderado)
  }
}*/
