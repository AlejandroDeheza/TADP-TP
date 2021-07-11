package Ale

import Ale.Utils.ResultadoRuleta
import org.scalactic.Tolerance.convertNumericToPlusOrMinusWrapper
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers.be
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class Tests extends AnyFreeSpec {

  "Primera parte - Apuestas" - {
    val jugadaCompuesta = ApuestaCombinada[ResultadoRuleta](
      List(JugarAlRojo(25), JugarADocena(10, SegundaDocena), JugarAlNumero(30, 23))
    )

    "primer caso" in {
      jugadaCompuesta(3) should be(50)
    }

    "segundo caso" in {
      jugadaCompuesta(14) should be(80)
    }

    "tercer caso" in {
      jugadaCompuesta(23) should be(1160)
    }

  }

  "Punto 3 - Distribuciones: Crear las distribuciones de probabilidad de" - {
    "El juego de ‘cara o cruz’, donde hay 50% de chances de que salga Cara y 50% de que salga Cruz" in {
      val distribucion = new GeneradorDistribuciones().Equiprobable(SucesosCaraCruz.sucesos)
      distribucion.probabilidadDe(Cara) should be(0.50)
      distribucion.probabilidadDe(Cruz) should be(0.50)
    }

    "La ruleta, que tiene las mismas chances de que salga cualquiera de los 37 números" in {
      val distribucion = new GeneradorDistribuciones().Equiprobable(SucesosRuleta.sucesos)
      distribucion.probabilidadDe(0) should be(1 / 37.0 +- 0.0001)
      distribucion.probabilidadDe(17) should be(1 / 37.0 +- 0.0001)
      distribucion.probabilidadDe(36) should be(1 / 37.0 +- 0.0001)
    }

    "‘Cara o cruz’ pero con una moneda cargada, en este caso sale Cara 4 de cada 7 veces y Cruz las restantes" in {
      val lista: List[SucesoPonderado[ResultadoCaraCruz]] = List(SucesoPonderado(Cara, 4), SucesoPonderado(Cruz, 3))
      val distribucion = new GeneradorDistribuciones[ResultadoCaraCruz]().Ponderado(lista)
      distribucion.probabilidadDe(Cara) should be(0.5714 +- 0.0001)
      distribucion.probabilidadDe(Cruz) should be(0.4285 +- 0.0001)
    }
  }

  "Punto 4 - Permitir que un jugador juegue sucesivamente varios juegos" - {
    val apuestas = List(JugarACara(10), JugarAlNumero(15, 0))
    val distribucion = JuegosSucesivos(apuestas).apply(15)


    "cantidad de sucesos posibles" in {
      distribucion.sucesosPosibles().length should be(3)
    }

    "probabilidad de conseguir 550 pesos" in {
      distribucion.probabilidadDe(550) should be(0.0135 +- 0.0001)
    }

    "probabilidad de conseguir 10 pesos" in {
      distribucion.probabilidadDe(10) should be(0.4864 +- 0.0001)
    }

    "probabilidad de conseguir 5 pesos" in {
      distribucion.probabilidadDe(5) should be(0.500 +- 0.0001)
    }
  }

  "Punto 5 - jugadores" - {
    val apuestas1 = List(JugarACara(30), JugarACara(15))
    val apuestas2 = List(JugarAlNumero(20, 12))
    val apuestas3 = List(JugarADocena(15, SegundaDocena))
    val apuestas4 = List(JugarAlRojo(14), JugarAImpar(43), JugarACruz(13))

    val juegosSucesivos1 = JuegosSucesivos(apuestas1)
    val juegosSucesivos2 = JuegosSucesivos(apuestas2)
    val juegosSucesivos3 = JuegosSucesivos(apuestas3)
    val juegosSucesivos4 = JuegosSucesivos(apuestas4)

    val combinacionesDeJuegos = List(juegosSucesivos1, juegosSucesivos2, juegosSucesivos3, juegosSucesivos4)

    val jugador1 = Jugador(30, Racional())
    val jugador2 = Jugador(50, Arriesgado())
    val jugador3 = Jugador(90, Cauto(90))
    val jugador4 = Jugador(130, Inventado())

    "1" in {
      jugador1(combinacionesDeJuegos) should be(juegosSucesivos1)
    }

    "2" in {
      jugador2(combinacionesDeJuegos) should be(juegosSucesivos2)
    }

    /*"3" in {
      jugador3(combinacionesDeJuegos) should be(juegosSucesivos3)
    } // todo: revisar*/

    "4" in {
      jugador4(combinacionesDeJuegos) should be(juegosSucesivos4)
    }
  }
}
