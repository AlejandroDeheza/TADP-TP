package tests

import dominio._
import org.scalactic.Tolerance.convertNumericToPlusOrMinusWrapper
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers.be
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import util.Utils.ResultadoRuleta

class Tests extends AnyFreeSpec {

  "Primera parte - Apuestas" - {
    val juegoCompuesto = ApuestaCompuesta[ResultadoRuleta](
      List(ApuestaSimple(25, JugarAl(Rojo)), ApuestaSimple(10, ADocena(SegundaDocena)), ApuestaSimple(30, AlNumero(23)))
    )

    "primer caso" in {
      juegoCompuesto(3) should be(50)
    }

    "segundo caso" in {
      juegoCompuesto(14) should be(80)
    }

    "tercer caso" in {
      juegoCompuesto(23) should be(1160)
    }

  }

  "Punto 3 - Distribuciones: Crear las distribuciones de probabilidad de" - {
    "El juego de ‘cara o cruz’, donde hay 50% de chances de que salga Cara y 50% de que salga Cruz" in {
      val distribucion = CaraCruz.distribucionResultados
      distribucion.probabilidadDe(Cara) should be(0.50)
      distribucion.probabilidadDe(Cruz) should be(0.50)
    }

    "La ruleta, que tiene las mismas chances de que salga cualquiera de los 37 números" in {
      val distribucion = Ruleta.distribucionResultados
      distribucion.probabilidadDe(0) should be(1 / 37.0 +- 0.0001)
      distribucion.probabilidadDe(17) should be(1 / 37.0 +- 0.0001)
      distribucion.probabilidadDe(36) should be(1 / 37.0 +- 0.0001)
    }

    "‘Cara o cruz’ pero con una moneda cargada, en este caso sale Cara 4 de cada 7 veces y Cruz las restantes" in {
      val lista: List[SucesoPonderado[ResultadoCaraCruz]] = List(SucesoPonderado(Cara, 4), SucesoPonderado(Cruz, 3))
      val distribucion = GeneradorDistribuciones(Ponderados(lista))
      distribucion.probabilidadDe(Cara) should be(0.5714 +- 0.0001)
      distribucion.probabilidadDe(Cruz) should be(0.4285 +- 0.0001)
    }
  }

  "Punto 4 - Permitir que un jugador juegue sucesivamente varios juegos" - {
    val juegos = List(ApuestaSimple(10, JugadaCaraCruz(Cara)), ApuestaSimple(15, AlNumero(0)))
    val distribucion = ApuestasSucesivas(juegos).apply(15)

    "cantidad de sucesos posibles" in {
      distribucion.sucesosPosibles.length should be(3)
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
    val juegos1 = List(ApuestaSimple(30, JugadaCaraCruz(Cara)), ApuestaSimple(15, JugadaCaraCruz(Cara)))
    val juegos2 = List(ApuestaSimple(20, AlNumero(12)))
    val juegos3 = List(ApuestaSimple(15, ADocena(SegundaDocena)))
    val juegos4 = List(ApuestaSimple(14, JugarAl(Rojo)), ApuestaSimple(43, JugarAl(Impar)), ApuestaSimple(13, JugadaCaraCruz(Cruz)))

    val juegosSucesivos1 = ApuestasSucesivas(juegos1)
    val juegosSucesivos2 = ApuestasSucesivas(juegos2)
    val juegosSucesivos3 = ApuestasSucesivas(juegos3)
    val juegosSucesivos4 = ApuestasSucesivas(juegos4)

    val combinacionesDeJuegos = List(juegosSucesivos1, juegosSucesivos2, juegosSucesivos3, juegosSucesivos4)

    val jugador1 = Jugador(30, Racional)
    val jugador2 = Jugador(50, Arriesgado)
    val jugador3 = Jugador(90, Cauto)
    val jugador4 = Jugador(130, Inventado)

    "1" in {
      jugador1(combinacionesDeJuegos).getOrElse(None) should be((juegosSucesivos1, juegosSucesivos1(30)))
    }

    "2" in {
      jugador2(combinacionesDeJuegos).getOrElse(None) should be((juegosSucesivos2, juegosSucesivos2(50)))
    }

    /*"3" in {
      jugador3(combinacionesDeJuegos).getOrElse(None)  should be((juegosSucesivos3, juegosSucesivos3(90)))
    } // todo: revisar*/

    "4" in {
      jugador4(combinacionesDeJuegos).getOrElse(None)  should be((juegosSucesivos4, juegosSucesivos4(130)))
    }
  }
}
