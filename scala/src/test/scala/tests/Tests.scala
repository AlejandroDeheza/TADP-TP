package tests

import dominio._
import org.scalactic.Tolerance.convertNumericToPlusOrMinusWrapper
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers.be
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import util.Utils.ResultadoRuleta

class Tests extends AnyFreeSpec {

  "Punto 1 - Jugadas y Apuestas" - {

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

  "Punto 2 - Apuestas compuestas" - {
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

  "Punto 3 - Distribuciones" - {

    "Distribucion CaraCruz, donde hay 50% de chances de que salga Cara y 50% de que salga Cruz" in {
      val distribucion = CaraCruz.distribucionResultados
      distribucion.probabilidadDe(Cara) should be(0.50)
      distribucion.probabilidadDe(Cruz) should be(0.50)

      distribucion.sucesos.map(_.probabilidad).sum should be(1.0 +- 0.0001)
    }

    "Distribucion Ruleta, que tiene las mismas chances de que salga cualquiera de los 37 números" in {
      val distribucion = Ruleta.distribucionResultados
      distribucion.probabilidadDe( 0) should be(1 / 37.0 +- 0.0001)
      distribucion.probabilidadDe(17) should be(1 / 37.0 +- 0.0001)
      distribucion.probabilidadDe(36) should be(1 / 37.0 +- 0.0001)

      distribucion.sucesos.map(_.probabilidad).sum should be(1.0 +- 0.0001)
    }

    "Distribucion CaraCruz pero con una moneda cargada, en este caso sale Cara 4 de cada 7 veces y Cruz las restantes" in {
      val lista: List[SucesoPonderado[ResultadoCaraCruz]] = List(SucesoPonderado(Cara, 4), SucesoPonderado(Cruz, 3))
      val distribucion = GeneradorDistribuciones(Ponderados(lista))
      distribucion.probabilidadDe(Cara) should be(0.5714 +- 0.0001)
      distribucion.probabilidadDe(Cruz) should be(0.4285 +- 0.0001)

      distribucion.sucesos.map(_.probabilidad).sum should be(1.0 +- 0.0001)
    }

    "Sucesos posibles" in {
      val lista: List[SucesoPonderado[ResultadoCaraCruz]] = List(SucesoPonderado(Cara, 4), SucesoPonderado(Cruz, 0))
      val distribucion = GeneradorDistribuciones(Ponderados(lista))
      distribucion.probabilidadDe(Cara) should be(1.0 +- 0.0001)
      distribucion.probabilidadDe(Cruz) should be(0.0 +- 0.0001)

      distribucion.sucesos.map(_.probabilidad).sum should be(1.0 +- 0.0001)
      distribucion.sucesosPosibles.length should be(1)
      distribucion.sucesosPosibles.head should be(Cara)
    }
  }

  "Punto 4 - ApuestasSucesivas" - {
    val apuestas = List(ApuestaSimple(10, CaraCruz(Cara)), ApuestaSimple(15, Ruleta(Al(0))))
    val distribucion = ApuestasSucesivas(apuestas).apply(15)
    distribucion.sucesos.head.historial.length should be(2)

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

    "lista apuestas vacia" in {
      val distribucion = ApuestasSucesivas(List()).apply(15)
      distribucion.sucesosPosibles.length should be(1)
      distribucion.sucesosPosibles.head should be(15)
    }

    "ampliarDistribucion() si no juega" in {
      val distribucion = ApuestasSucesivas(List()).apply(15)
      val apuesta = ApuestaSimple(20, CaraCruz(Cruz))
      val otraDistribucion = apuesta.ampliarDistribucion(distribucion)
      otraDistribucion.sucesos.length should be(1)
      otraDistribucion.sucesosPosibles.length should be(1)
      otraDistribucion.sucesosPosibles.head should be(15)
      otraDistribucion.probabilidadDe(15) should be(1.0 +- 0.0001)

      otraDistribucion.sucesos.head.historial.length should be(1)
      otraDistribucion.sucesos.head.historial.head should be(NoJugo(apuesta))
    }

    "ampliarDistribucion() si gana o pierde" in {
      val distribucion = ApuestasSucesivas(List()).apply(25)
      val apuesta = ApuestaSimple(20, CaraCruz(Cruz))
      val otraDistribucion = apuesta.ampliarDistribucion(distribucion)
      otraDistribucion.sucesos.length should be(2)
      otraDistribucion.sucesosPosibles.length should be(2)
      otraDistribucion.probabilidadDe(45) should be(0.5 +- 0.0001)
      otraDistribucion.probabilidadDe( 5) should be(0.5 +- 0.0001)
      otraDistribucion.probabilidadDe( 0) should be(0.0 +- 0.0001)
    }

    "ampliarDistribucion() con las jugadas de ruleta" in {
      val distribucion = ApuestasSucesivas(List()).apply(20)
      val apuesta1 = ApuestaSimple(20, Ruleta(Rojo))
      val apuesta2 = ApuestaSimple(20, Ruleta(Negro))
      val apuesta3 = ApuestaSimple(20, Ruleta(Par))
      val apuesta4 = ApuestaSimple(20, Ruleta(Impar))
      val apuesta5 = ApuestaSimple(20, Ruleta(SegundaDocena))
      val apuesta6 = ApuestaSimple(20, Ruleta(Al(4)))

      val otraDistribucion1 = apuesta1.ampliarDistribucion(distribucion)
      otraDistribucion1.sucesosPosibles.length should be(2)
      otraDistribucion1.probabilidadDe(40) should be(18/37.0 +- 0.0001)
      otraDistribucion1.probabilidadDe( 0) should be(19/37.0 +- 0.0001)
      otraDistribucion1.sucesos.head.historial.length should be(1)

      val otraDistribucion2 = apuesta2.ampliarDistribucion(distribucion)
      otraDistribucion2.sucesosPosibles.length should be(2)
      otraDistribucion2.probabilidadDe(40) should be(18/37.0 +- 0.0001)
      otraDistribucion2.probabilidadDe( 0) should be(19/37.0 +- 0.0001)
      otraDistribucion2.sucesos.head.historial.length should be(1)

      val otraDistribucion3 = apuesta3.ampliarDistribucion(distribucion)
      otraDistribucion3.sucesosPosibles.length should be(2)
      otraDistribucion3.probabilidadDe(40) should be(18/37.0 +- 0.0001)
      otraDistribucion3.probabilidadDe( 0) should be(19/37.0 +- 0.0001)
      otraDistribucion3.sucesos.head.historial.length should be(1)

      val otraDistribucion4 = apuesta4.ampliarDistribucion(distribucion)
      otraDistribucion4.sucesosPosibles.length should be(2)
      otraDistribucion4.probabilidadDe(40) should be(18/37.0 +- 0.0001)
      otraDistribucion4.probabilidadDe( 0) should be(19/37.0 +- 0.0001)
      otraDistribucion4.sucesos.head.historial.length should be(1)

      val otraDistribucion5 = apuesta5.ampliarDistribucion(distribucion)
      otraDistribucion5.sucesosPosibles.length should be(2)
      otraDistribucion5.probabilidadDe(60) should be(12/37.0 +- 0.0001)
      otraDistribucion5.probabilidadDe( 0) should be(25/37.0 +- 0.0001)
      otraDistribucion5.sucesos.head.historial.length should be(1)

      val otraDistribucion6 = apuesta6.ampliarDistribucion(distribucion)
      otraDistribucion6.sucesosPosibles.length should be(2)
      otraDistribucion6.probabilidadDe(720) should be(1/37.0 +- 0.0001)
      otraDistribucion6.probabilidadDe(  0) should be(36/37.0 +- 0.0001)
      otraDistribucion6.sucesos.head.historial.length should be(1)
    }

    "ApuestaSucesiva con ApuestaCompuesta" in {
      val distribucionVacia = ApuestasSucesivas(List()).apply(15)
      val apuestas = List(ApuestaSimple(10, CaraCruz(Cara)), ApuestaSimple(15, CaraCruz(Cruz)))

      val apuestasCompuestas = List(ApuestaCompuesta(apuestas))
      val distribucion1 = ApuestasSucesivas(apuestasCompuestas).apply(15)
      val distribucion2 = ApuestasSucesivas(apuestas).apply(15)
      val distribucion3 = ApuestaCompuesta(apuestas).ampliarDistribucion(distribucionVacia)

      distribucion1 should be(distribucion2)
      distribucion1 should be(distribucion3)
      distribucion2 should be(distribucion3)
    }
  }

  "Punto 5 - jugadores" - {
    val apuestas1 = List(ApuestaSimple(30, CaraCruz(Cara)), ApuestaSimple(15, CaraCruz(Cara)))
    val apuestas2 = List(ApuestaSimple(20, Ruleta(Al(12))))
    val apuestas3 = List(ApuestaSimple(14, Ruleta(Rojo)), ApuestaSimple(43, Ruleta(Impar)), ApuestaSimple(13, CaraCruz(Cruz)))

    val apuestasSucesivos1 = ApuestasSucesivas(apuestas1)
    val apuestasSucesivos2 = ApuestasSucesivas(apuestas2)
    val apuestasSucesivos3 = ApuestasSucesivas(apuestas3)

    val combinacionesDeApuestas = List(apuestasSucesivos1, apuestasSucesivos2, apuestasSucesivos3)

    val jugador1 = Jugador(30, Racional)
    val jugador2 = Jugador(50, Arriesgado)
    val jugador3 = Jugador(90, Cauto)
    val jugador4 = Jugador(130, Inventado)
    val jugadorCustom = Jugador(100, CriterioCustom(distribucion => distribucion.sucesos.length))

    "Racional" in {
      jugador1(combinacionesDeApuestas) should be((apuestasSucesivos1, apuestasSucesivos1(30)))
    }

    "Arriesgado" in {
      jugador2(combinacionesDeApuestas) should be((apuestasSucesivos2, apuestasSucesivos2(50)))
    }

    "Cauto" in {
      jugador3(combinacionesDeApuestas) should be((apuestasSucesivos1, apuestasSucesivos1(90)))
    }

    "Inventado" in {
      jugador4(combinacionesDeApuestas) should be((apuestasSucesivos3, apuestasSucesivos3(130)))
    }

    "Custom" in {
      jugadorCustom(combinacionesDeApuestas) should be((apuestasSucesivos3, apuestasSucesivos3(100)))
    }
  }
}
