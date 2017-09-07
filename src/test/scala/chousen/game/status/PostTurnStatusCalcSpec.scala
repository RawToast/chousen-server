package chousen.game.status

import chousen.api.data._
import org.scalatest.WordSpec

class PostTurnStatusCalcSpec extends WordSpec {

  "PostTurnStatusCalc" when {
    val calc = new PostTurnStatusCalc {}
    val gameState = GameStateGenerator.staticGameState
    val standardAction = Fireball

    val newState = calc.applyStatusEffects(gameState, standardAction)

    "The Player has no status effects" should {
      "Make no changes" in {
        assert(gameState == newState)
      }
    }


    "The Player has the Regen status effect" should {
      val gameState = GameStateGenerator.staticGameState

      import chousen.Optics._

      lazy val initialState: GameState = PlayerLens.composeLens(PlayerStatusLens).set(Seq(StatusBuilder.makeRegen(5)))
        .andThen(PlayerLens.composeLens(PlayerHealthLens).set(20)).apply(gameState)


      lazy val resultState = calc.applyStatusEffects(initialState, standardAction)

      "Create a new message" in {
        assert(resultState.messages.size > initialState.messages.size)
      }

      "Heal the player" in {
        assert(resultState.player.stats.currentHp > initialState.player.stats.currentHp)
      }

      "Reduce the the length of the effect" in {
        assert(getStatusLength(initialState) > 1)
        assert(getStatusLength(resultState) > 1)

        assert(getStatusLength(initialState) > getStatusLength(resultState))
      }

      "Not heal when a CardAction is played" in {
        lazy val cardResult = calc.applyStatusEffects(initialState, Miracle)

        assert(cardResult.player.stats.currentHp == initialState.player.stats.currentHp)
      }

      "Not heal when a CampAction is played" in {
        lazy val cardResult = calc.applyStatusEffects(initialState, RestAndExplore)

        assert(cardResult.player.stats.currentHp == initialState.player.stats.currentHp)
      }
    }

    "The Player uses a CardAction" should {
      val gameState = GameStateGenerator.staticGameState

      import chousen.Optics._

      val initialState = PlayerLens.composeLens(PlayerStatusLens).set(Seq(StatusBuilder.makeHaste(5)))
        .andThen(PlayerLens.composeLens(PlayerHealthLens).set(20)).apply(gameState)


      lazy val newState = calc.applyStatusEffects(initialState, Rummage)


      "Not reduce the the length of any effects" in {
        assert(getStatusLength(newState) == getStatusLength(initialState))
      }
    }

  }
  private def getStatusLength(bb: GameState): Int = bb.player.status.headOption.map(_.turns).getOrElse(0)
}

