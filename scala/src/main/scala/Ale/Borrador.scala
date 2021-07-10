package Ale

import scala.language.implicitConversions

// 3 - Crear las distribuciones de probabilidad de:
// a - El juego de ‘cara o cruz’, donde hay 50% de chances de que salga Cara y 50% de que salga Cruz.
// b - La ruleta, que tiene las mismas chances de que salga cualquiera de los 37 números.
// c - ‘Cara o cruz’ pero con una moneda cargada, en este caso sale Cara 4 de cada 7 veces y Cruz las restantes.

// De una distribución de probabilidad queremos poder saber:
// - sus sucesos posibles, que sería el conjunto de sucesos de la distribución sin su probabilidad (si un suceso tiene
// probabilidad 0 no debería estar acá).
// - la probabilidad de un suceso en particular.
//
// Queremos tener algunas facilidades para construir distribuciones de probabilidad de diferentes maneras:
// - Evento Seguro: dado un suceso, es una distribución que sólo contiene ese suceso con probabilidad de 100%.
// - Distribución equiprobable: a partir de una lista de sucesos genera una distribución donde cada suceso tiene la misma
// probabilidad de ocurrir.
// - Distribución a partir de valores ponderados: dada una lista de sucesos y el “peso” de cada suceso,
// se obtiene una distribución. Ejemplo: si tenemos un test horrible y no determinístico que falla 2 de cada 3 veces,
// deberíamos poder construir la distribución a partir de esos valores en vez de tener que calcular la distribución a
// mano. Entonces, a partir de TestPasa con peso 1 y TestFalla con peso 2, obtendríamos una distribución con el suceso
// TestPasa con probabilidad 66.66% y TestFalla con probabilidad 33.34%.

/*case class GeneradorDistribuciones() {
  def eventoSeguro[T <: ResultadoDeJuego](suceso: T): DistribucionProbabilidad[T] = {
    DistribucionProbabilidad(List(SucesoConProbabilidad(suceso, 100.0)))
  }
  def distribucionEquiprobable[T <: ResultadoDeJuego](sucesos: List[T]): DistribucionProbabilidad[T] = {
    val cantidad: Int = sucesos.length
    DistribucionProbabilidad(sucesos.map(suceso => SucesoConProbabilidad(suceso, 100.0 / cantidad)))
  }
  def distribucionPonderada[T <: ResultadoDeJuego](sucesos: List[SucesoPonderado[T]]): DistribucionProbabilidad[T] = {
    val pesoTotal: Int = sucesos.map(_.pesoPonderado).sum
    DistribucionProbabilidad(sucesos.map(_.pasarASucesoProbable(pesoTotal)))
  }
}*/

/*case class GeneradorEventoSeguro[T <: ResultadoDeJuego](suceso: SucesoBasico[T]) {
  def apply(): DistribucionProbabilidad[T] = {
    DistribucionProbabilidad(List(SucesoConProbabilidad(suceso.resultadoDeJuego, 100.0)))
  }
}

case class GeneradorEquiprobable[T <: ResultadoDeJuego](sucesos: List[SucesoBasico[T]]) {
  def apply(): DistribucionProbabilidad[T] = {
    val cantidad: Int = sucesos.length
    DistribucionProbabilidad(sucesos.map(_.pasarASucesoProbable(cantidad)))
  }
}

case class GeneradorPonderado[T <: ResultadoDeJuego](sucesos: List[SucesoPonderado[T]]) {
  def apply(): DistribucionProbabilidad[T] = {
    val pesoTotal: Int = sucesos.map(_.pesoPonderado).sum
    DistribucionProbabilidad(sucesos.map(_.pasarASucesoProbable(pesoTotal)))
  }
}*/



/*case object SucesosCaraCruz {
  val sucesos: List[ResultadoCaraCruz] = List(Cara, Cruz)
}
case object SucesosRuleta {
  val sucesos: List[ResultadoRuleta] = (0 to 36).map(i => ResultadoRuleta(i)).toList
}



case class SucesoPonderado[T <: ResultadoDeJuego](resultadoDeJuego: T, pesoPonderado: Int) {
  def pasarASucesoProbable(pesoTotal: Int): SucesoConProbabilidad[T] = {
    SucesoConProbabilidad(resultadoDeJuego, (100.0 / pesoTotal) * pesoPonderado)
  }
}

case class SucesoConProbabilidad[T <: ResultadoDeJuego](resultadoDeJuego: T, probabilidad: Double)

case class DistribucionProbabilidad[T <: ResultadoDeJuego](distribucion: List[SucesoConProbabilidad[T]]) {
  def sucesosPosibles() : List[T] = distribucion.filter(_.probabilidad > 0.0).map(_.resultadoDeJuego)

  def probabilidadDe(suceso: T) :Double = {
    distribucion.find(su => su.resultadoDeJuego == suceso) match {
      case Some(resultado) => resultado.probabilidad
      case _ => 0.0
    }
  }*/

  /*def tieneEventoSeguro(): Boolean = distribucion.length == 1

  def esEquiprobable(): Boolean = {
    import Ale.comparadorDoubles.Comparador
    val unaProbabilidad = distribucion.head.probabilidad
    distribucion.forall(_.probabilidad ~= unaProbabilidad)
  }
}*/



/*// JUEGOS SUCESIVOS <<------------------------------
case class JuegosSucesivos(apuestas: List[Apuesta]) {
  private def listaSinCabeza(): List[Apuesta] = apuestas match {
    case ::(_, next) => next
    case Nil => List()
  }

  private def siguiente(): List[Apuesta] = listaSinCabeza() match {
    case ::(_, next) => next
    case Nil => List()
  }

  def apply(montoInicial: Int): DistribucionProbabilidad = {
    apuestas.foldLeft(DistribucionProbabilidad(List())) {
      (distribucion, apuesta) => apuesta match {
          case simple: ApuestaSimple =>
            if (listaSinCabeza().nonEmpty && montoInicial > simple.montoApostado) {
              simple.distribucionDeGanancias().distribucion
                .map(_.sucesoGenerico.asInstanceOf[Ganancia].monto)
                .flatMap(ganancia => distribucion ++ JuegosSucesivos(listaSinCabeza()).apply(montoInicial - simple.montoApostado + ganancia))
            } else if (listaSinCabeza().nonEmpty) {
              JuegosSucesivos(listaSinCabeza()).apply(montoInicial) ++ distribucion ++ GeneradorDistribuciones().eventoSeguro(Ganancia(montoInicial))
            } else if (montoInicial > simple.montoApostado) {
              simple.distribucionDeGanancias() ++ distribucion
            } else {
              distribucion
            }
          case combinada: ApuestaCombinada =>
        }
    }

    apuestas.foldLeft(montoInicial) {
    (monto, apuesta) => apuesta match {
        case simple: ApuestaSimple =>
          val montoActual = monto - simple.montoApostado
          val montos = simple.distribucionDeGanancias().distribucion.map(_.sucesoGenerico.monto + montoActual)

        match {
          case caraCruz: ApuestaCaraCruz => caraCruz match {
            case DuplicarSiSaleCara(montoApostado) => caraCruz.distribucionDeGanancias().sucesosPosibles()
            case DuplicarSiSaleCruz(montoApostado) =>
          }
          case ruleta: ApuestaRuleta => ruleta match {
            case JugarAlRojo(montoApostado) => ruleta.distribucionDeGanancias()
            case JugarAlNegro(montoApostado) =>
            case JugarAlNumero(montoApostado, numeroApostado) =>
            case JugarAPar(montoApostado) =>
            case JugarAImpar(montoApostado) =>
            case JugarADocena(montoApostado, docenaElegida) =>
          }
        }
        case combinada: ApuestaCombinada => combinada match {
          case CaraCruzCombinada(apuestas) =>
          case RuletaCombinada(apuestas) =>
          case _ =>
        }
      }
  }
  }
}*/


/*// JUEGOS SUCESIVOS <<------------------------------
case class JuegosSucesivos(apuestas: List[Apuesta]) {

  // sucesoPadre inicial --> SucesoConProbabilidad(Ganancia(montoInicial), 100.0)
  def apply(montoInicial: Int, sucesoPadre: SucesoConProbabilidad): DistribucionProbabilidad = {
    apuestas.foldLeft(DistribucionProbabilidad(List())) {
      (distribucion, apuesta) => apuesta match {
          case simple: ApuestaSimple =>
            if (listaSinCabeza().nonEmpty && montoInicial >= simple.montoApostado) {
              simple.distribucionDeGanancias().distribucion
                .map(_.sucesoGenerico.asInstanceOf[Ganancia].monto)
                .map(ganancia => JuegosSucesivos(listaSinCabeza()).apply(montoInicial - simple.montoApostado + ganancia, sucesoPadre))
                .reduce((dist1, dist2) => dist1 ++ dist2)
            } else if (listaSinCabeza().isEmpty && montoInicial >= simple.montoApostado) {
              simple.distribucionDeGanancias() * (sucesoPadre, simple.montoApostado)
            } else if (siguiente().nonEmpty) {
              JuegosSucesivos(siguiente()).apply(montoInicial,
                GeneradorDistribuciones().eventoSeguro(Ganancia(montoInicial)).distribucion.head) * sucesoPadre
            }  else {
              GeneradorDistribuciones().eventoSeguro(Ganancia(montoInicial)) * sucesoPadre
            }
          case combinada: ApuestaCombinada => DistribucionProbabilidad(List()) // TODO: REVISAR
        }
    }
  }

  private def listaSinCabeza(): List[Apuesta] = apuestas match {
    case ::(_, next) => next
    case Nil => List()
  }

  private def siguiente(): List[Apuesta] = listaSinCabeza() match {
    case ::(_, next) => next
    case Nil => List()
  }
}*/


/*val lista3 = otraLista.map(suce => SucesoConProbabilidad(
      Ganancia(suce.sucesoGenerico.asInstanceOf[Ganancia].monto),
      suce.probabilidad))
      */


/*// version sin fold, usando recursividad
  // sucesoPadre inicial --> SucesoConProbabilidad(Ganancia(montoInicial), 100.0)
  def apply2(montoInicial: Int, sucesoPadre: SucesoConProbabilidad): DistribucionProbabilidad = {
    apuestas.head match {
        case simple: ApuestaSimple =>
          if (listaSinCabeza().nonEmpty && montoInicial >= simple.montoApostado) {
            simple.distribucionDeGanancias().distribucion
              .map(_.sucesoGenerico.asInstanceOf[Ganancia].monto)
              .map(ganancia => JuegosSucesivos(listaSinCabeza()).apply2(montoInicial - simple.montoApostado + ganancia, sucesoPadre))
              .reduce((dist1, dist2) => dist1 ++ dist2)
          } else if (listaSinCabeza().isEmpty && montoInicial >= simple.montoApostado) {
            simple.distribucionDeGanancias() * (sucesoPadre, simple.montoApostado)
          } else if (siguiente().nonEmpty) {
            JuegosSucesivos(siguiente()).apply2(montoInicial,
              GeneradorDistribuciones().eventoSeguro(Ganancia(montoInicial)).distribucion.head) * sucesoPadre
          }  else {
            GeneradorDistribuciones().eventoSeguro(Ganancia(montoInicial)) * sucesoPadre
          }
        case combinada: ApuestaCombinada => DistribucionProbabilidad(List()) // TODO: REVISAR
      }
    }

  private def listaSinCabeza(): List[Apuesta] = apuestas match {
    case ::(_, next) => next
    case Nil => List()
  }

  private def siguiente(): List[Apuesta] = listaSinCabeza() match {
    case ::(_, next) => next
    case Nil => List()
  }*/




/*def ++(dist: DistribucionProbabilidad): DistribucionProbabilidad = {
    DistribucionProbabilidad(dist.copy().distribucion ++ copy().distribucion)
  }

  def *(suceso: SucesoConProbabilidad, montoApostado: Int): DistribucionProbabilidad = {
    DistribucionProbabilidad(
      copy().distribucion.map(
        su => SucesoConProbabilidad(
          Ganancia(
            suceso.sucesoGenerico.asInstanceOf[Ganancia].monto
              - montoApostado + su.sucesoGenerico.asInstanceOf[Ganancia].monto
          ), su.probabilidad * suceso.probabilidad
        )
      )
    )
  }

  def *(suceso: SucesoConProbabilidad): DistribucionProbabilidad = {
    DistribucionProbabilidad(
      copy().distribucion.map(
        su => SucesoConProbabilidad( su.sucesoGenerico, su.probabilidad * suceso.probabilidad )
      )
    )
  }*/