package Ale

import org.scalactic.Tolerance.convertNumericToPlusOrMinusWrapper
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers.be
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class Tests extends AnyFreeSpec {

  "Primera parte - Apuestas" - {
    val jugadaCompuesta: RuletaCombinada = RuletaCombinada(
      List(JugarAlRojo(25), JugarADocena(10, SegundaDocena), JugarAlNumero(30, 23))
    )

    "primer caso" in {
      jugadaCompuesta(ResultadoRuleta(3)) should be(Ganancia(50))
    }

    "segundo caso" in {
      jugadaCompuesta(ResultadoRuleta(14)) should be(Ganancia(80))
    }

    "tercer caso" in {
      jugadaCompuesta(ResultadoRuleta(23)) should be(Ganancia(1160))
    }

  }

  "Punto 3 - Distribuciones: Crear las distribuciones de probabilidad de" - {
    "El juego de ‘cara o cruz’, donde hay 50% de chances de que salga Cara y 50% de que salga Cruz" in {
      val distribucion = GeneradorDistribuciones().distribucionEquiprobable(SucesosCaraCruz.sucesos)
      distribucion.probabilidadDe(Cara) should be(0.50)
      distribucion.probabilidadDe(Cruz) should be(0.50)
    }

    "La ruleta, que tiene las mismas chances de que salga cualquiera de los 37 números" in {
      val distribucion = GeneradorDistribuciones().distribucionEquiprobable(SucesosRuleta.sucesos)
      distribucion.probabilidadDe(ResultadoRuleta(0)) should be(1/37.0 +- 0.0001)
      distribucion.probabilidadDe(ResultadoRuleta(17)) should be(1/37.0 +- 0.0001)
      distribucion.probabilidadDe(ResultadoRuleta(36)) should be(1/37.0 +- 0.0001)
    }

    "‘Cara o cruz’ pero con una moneda cargada, en este caso sale Cara 4 de cada 7 veces y Cruz las restantes" in {
      val lista = List(SucesoPonderado(Cara, 4), SucesoPonderado(Cruz, 3))
      val distribucion = GeneradorDistribuciones().distribucionPonderada(lista)
      distribucion.probabilidadDe(Cara) should be(0.5714 +- 0.0001)
      distribucion.probabilidadDe(Cruz) should be(0.4285 +- 0.0001)
    }
  }

  "Punto 4 - Permitir que un jugador juegue sucesivamente varios juegos" - {
    val apuestas = List(DuplicarSiSaleCara(10), JugarAlNumero(15, 0))
    val distribucion = JuegosSucesivos(apuestas).apply(15)


    "cantidad de sucesos posibles" in {
      distribucion.sucesosPosibles().length should be(3)
    }

    "probabilidad de conseguir 550 pesos" in {
      distribucion.probabilidadDe(Ganancia(550)) should be(0.0135 +- 0.0001)
    }

    "probabilidad de conseguir 10 pesos" in {
      distribucion.probabilidadDe(Ganancia(10)) should be(0.4864 +- 0.0001)
    }

    "probabilidad de conseguir 5 pesos" in {
      distribucion.probabilidadDe(Ganancia(5)) should be(0.500 +- 0.0001)
    }
  }
}
