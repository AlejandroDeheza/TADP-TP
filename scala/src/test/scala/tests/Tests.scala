package tests

import dominio._
import org.scalactic.Tolerance.convertNumericToPlusOrMinusWrapper
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers.be
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import util.Utils.ResultadoRuleta

class Tests extends AnyFreeSpec {

  "Primera parte - Jugadas y Apuestas" - {

    "Si utilizo la jugada “jugar a duplicar si sale cara” poniendo $20 y el resultado del juego es cara, obtengo $40," +
      " mientras que si uso la misma jugada con el mismo monto pero el resultado del juego es seca, obtengo $0" in {
      val apuesta = ApuestaSimple(20, CaraCruz(Cara))
      apuesta(Cara) should be(40)
      apuesta(Cruz) should be(0)
    }

    "jugadas de CaraCruz" in {
      CaraCruz(Cara).cumpleCon(Cara) should be(true)
      CaraCruz(Cruz).cumpleCon(Cruz) should be(true)
      CaraCruz(Cara).cumpleCon(Cruz) should be(false)
      CaraCruz(Cruz).cumpleCon(Cara) should be(false)
    }

    "jugadas de ruleta" in {
      Ruleta(Rojo).cumpleCon(Rojo.valores.last)    should be(true)
      Ruleta(Rojo).cumpleCon(Rojo.valores(5))      should be(true)
      Ruleta(Negro).cumpleCon(Negro.valores(5))    should be(true)
      Ruleta(Rojo).cumpleCon(Negro.valores.last)   should be(false)
      Ruleta(Rojo).cumpleCon(Negro.valores(5))     should be(false)
      Ruleta(Negro).cumpleCon(Rojo.valores(5))     should be(false)
      Ruleta(Rojo).cumpleCon(0)    should be(false)
      Ruleta(Negro).cumpleCon(0)   should be(false)

      Ruleta(Par).cumpleCon(14)    should be(true)
      Ruleta(Impar).cumpleCon(15)  should be(true)
      Ruleta(Par).cumpleCon(0)     should be(false)
      Ruleta(Impar).cumpleCon(0)   should be(false)

      Ruleta(SegundaDocena).cumpleCon(15) should be(true)
      Ruleta(SegundaDocena).cumpleCon(10) should be(false)
      Ruleta(SegundaDocena).cumpleCon(0)  should be(false)

      Ruleta(Al(23)).cumpleCon(23) should be(true)
      Ruleta(Al(23)).cumpleCon(14) should be(false)
      Ruleta(Al(0)).cumpleCon(0)   should be(true)
    }

    "Ganancias obtenidas de ApuestaSimple" in {
      val montoApostado = 10
      val apuesta1 = ApuestaSimple(montoApostado, CaraCruz(Cara))
      val apuesta2 = ApuestaSimple(montoApostado, CaraCruz(Cruz))
      val apuesta3 = ApuestaSimple(montoApostado, Ruleta(Rojo))
      val apuesta4 = ApuestaSimple(montoApostado, Ruleta(Negro))
      val apuesta5 = ApuestaSimple(montoApostado, Ruleta(Par))
      val apuesta6 = ApuestaSimple(montoApostado, Ruleta(Impar))
      val apuesta7 = ApuestaSimple(montoApostado, Ruleta(PrimerDocena))
      val apuesta8 = ApuestaSimple(montoApostado, Ruleta(Al(3)))

      apuesta1(Cara)              should be(montoApostado * 2)
      apuesta2(Cruz)              should be(montoApostado * 2)
      apuesta3(Rojo.valores(5))   should be(montoApostado * 2)
      apuesta4(Negro.valores(5))  should be(montoApostado * 2)
      apuesta5(16)                should be(montoApostado * 2)
      apuesta6(17)                should be(montoApostado * 2)
      apuesta7(3)                 should be(montoApostado * 3)
      apuesta8(3)                 should be(montoApostado * 36)
    }
  }

  "Segunda parte - Apuestas compuestas" - {
    val apuestaCompuesta = ApuestaCompuesta[ResultadoRuleta](
      List(ApuestaSimple(25, Ruleta(Rojo)), ApuestaSimple(10, Ruleta(SegundaDocena)), ApuestaSimple(30, Ruleta(Al(23))))
    )

    "primer caso" in {
      apuestaCompuesta(3) should be(50)
    }

    "segundo caso" in {
      apuestaCompuesta(14) should be(80)
    }

    "tercer caso" in {
      apuestaCompuesta(23) should be(1160)
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
    val juegos = List(ApuestaSimple(10, CaraCruz(Cara)), ApuestaSimple(15, Ruleta(Al(0))))
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
    val juegos1 = List(ApuestaSimple(30, CaraCruz(Cara)), ApuestaSimple(15, CaraCruz(Cara)))
    val juegos2 = List(ApuestaSimple(20, Ruleta(Al(12))))
    val juegos3 = List(ApuestaSimple(15, Ruleta(SegundaDocena)))
    val juegos4 = List(ApuestaSimple(14, Ruleta(Rojo)), ApuestaSimple(43, Ruleta(Impar)), ApuestaSimple(13, CaraCruz(Cruz)))

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
      jugador4(combinacionesDeJuegos).getOrElse(None) should be((juegosSucesivos4, juegosSucesivos4(130)))
    }
  }
}
