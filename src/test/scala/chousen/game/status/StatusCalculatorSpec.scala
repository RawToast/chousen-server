package chousen.game.status

import chousen.api.data.GameStateGenerator.playerName
import chousen.api.data.{CharStats, Player}
import org.scalatest.WordSpec


class StatusCalculatorSpec extends WordSpec {

  val statusCalculator = new StatusCalculator()
  val basicPlayer = Player(playerName, CharStats(100, 100), 1)


  "StatusCalculator" when {

    "Calculating Player state" must {

      "Return the same values when the player has no status effects" in {

        val result = statusCalculator.calculate(basicPlayer)

        assert(result == basicPlayer)

      }

    }

  }

}
