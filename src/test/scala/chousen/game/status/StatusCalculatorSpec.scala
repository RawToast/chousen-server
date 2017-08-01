package chousen.game.status

import chousen.api.data.GameStateGenerator.playerName
import chousen.api.data.{CharStats, Experience, Player, Status}
import org.scalatest.WordSpec


class StatusCalculatorSpec extends WordSpec {

  val statusCalculator = new StatusCalculator()
  val basicPlayer = Player(playerName, "test", CharStats(100, 100), Experience(), 1, Seq.empty)

  def playerWithStatus(status: Status) = {
    basicPlayer.copy(status = Seq(status))
  }

  "StatusCalculator" when {

    "Calculating Player state" must {

      "Return the same values when the player has no status effects" in {

        val result = statusCalculator.calculate(basicPlayer)

        assert(result == basicPlayer)

      }

      "Return a different player when the player has a status effect" in {

        val playerWithHaste: Player = playerWithStatus(StatusBuilder.makeHaste(4))
        val result = statusCalculator.calculate(playerWithHaste)

        assert(result != playerWithHaste)

      }

    }

    "Calculating a Player affected by Haste" must {

      val playerWithEffect: Player = playerWithStatus(StatusBuilder.makeHaste(4))
      val resultPlayer = statusCalculator.calculate(playerWithEffect)

      "Return a player with different values" in {
        assert(resultPlayer != playerWithEffect)
      }

      "Increase the player's speed" in {
        assert(resultPlayer.stats.speed > playerWithEffect.stats.speed)
      }
    }

    "Calculating a Player affected by StoneSkin" must {

      val playerWithEffect: Player = playerWithStatus(StatusBuilder.makeStoneSkin(4))
      val resultPlayer = statusCalculator.calculate(playerWithEffect)

      "Return a player with different values" in {
        assert(resultPlayer != playerWithEffect)
      }

      "Increase the player's vitality" in {
        assert(resultPlayer.stats.vitality > playerWithEffect.stats.vitality)
      }
    }

    "Calculating a Player affected by Might" must {

      val playerWithEffect: Player = playerWithStatus(StatusBuilder.makeMight(4))
      val resultPlayer = statusCalculator.calculate(playerWithEffect)

      "Return a player with different values" in {
        assert(resultPlayer != playerWithEffect)
      }

      "Increase the player's strength" in {
        assert(resultPlayer.stats.strength > playerWithEffect.stats.strength)
      }
    }

    "Calculating a Player affected by Dexterity" must {

      val playerWithEffect: Player = playerWithStatus(StatusBuilder.makeDexterity(4))
      val resultPlayer = statusCalculator.calculate(playerWithEffect)

      "Return a player with different values" in {
        assert(resultPlayer != playerWithEffect)
      }

      "Increase the player's dexterity" in {
        assert(resultPlayer.stats.dexterity > playerWithEffect.stats.dexterity)
      }
    }

    "Calculating a Player affected by Smart" must {

      val playerWithEffect: Player = playerWithStatus(StatusBuilder.makeSmart(4))
      val resultPlayer = statusCalculator.calculate(playerWithEffect)

      "Return a player with different values" in {
        assert(resultPlayer != playerWithEffect)
      }

      "Increase the player's intellect" in {
        assert(resultPlayer.stats.intellect > playerWithEffect.stats.intellect)
      }
    }

    "Calculating a Player affected by Berserk" must {

      val playerWithEffect: Player = playerWithStatus(StatusBuilder.makeBerserk(4))
      val resultPlayer = statusCalculator.calculate(playerWithEffect)

      "Return a player with different values" in {
        assert(resultPlayer != playerWithEffect)
      }

      "Increase the player's strength" in {
        assert(resultPlayer.stats.strength > playerWithEffect.stats.strength)
      }

      "Increase the player's speed" in {
        assert(resultPlayer.stats.speed > playerWithEffect.stats.speed)
      }
    }

  }

}
