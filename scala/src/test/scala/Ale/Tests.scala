package Ale

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers.be
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class Tests extends AnyFreeSpec{
  val jugadaCompuesta: JugadaCompuesta = JugadaCompuesta(
    List(JugarAlRojo(25), JugarADocena(10, SegundaDocena), JugarAlNumero(30, 23))
  )

  "Primera parte" - {
    "Apuestas" - {
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
  }
}
