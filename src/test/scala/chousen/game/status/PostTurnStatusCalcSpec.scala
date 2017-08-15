package chousen.game.status

import chousen.api.data.GameStateGenerator
import org.scalatest.WordSpec

class PostTurnStatusCalcSpec extends WordSpec{

  "PostTurnStatusCalc" when {
    val calc = new PostTurnStatusCalc {}

    "The Player has no status effects" should {
        val gameState = GameStateGenerator.staticGameState
        val newState = calc.applyStatusEffects(gameState)

      "Make no changes" in {
        assert(gameState == newState)
      }

    }


    "The Player has the Regen status effect" should {
      val gameState = GameStateGenerator.staticGameState

      import chousen.Optics._

      val initialState = PlayerLens.composeLens(PlayerStatusLens).set(Seq(StatusBuilder.makeRegen(5)))
          .andThen(PlayerLens.composeLens(PlayerHealthLens).set(20)).apply(gameState)


      val newState = calc.applyStatusEffects(initialState)

      "Create new game Messages" in {
        assert(newState.messages.size > initialState.messages.size)
      }

      "Heal the player" in {
        assert(newState.player.stats.currentHp > initialState.player.stats.currentHp)
      }
    }

  }

}
