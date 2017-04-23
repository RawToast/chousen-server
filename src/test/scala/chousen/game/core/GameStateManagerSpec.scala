package chousen.game.core

import chousen.api.core.data.GameStateGenerator
import chousen.api.data.GameState
import org.scalatest.WordSpec

class GameStateManagerSpec extends WordSpec {

  "GameStateManager" when {

    val gameStateManager = GameStateManager

    "Creating a game" should {

      "Set the player's name" in {
        val game = gameStateManager.create("Bob")

        assert(game.player.name == "Bob")
      }

      "Set a different ID on each create" in {
        val game1 = gameStateManager.create("Bob")
        val game2 = gameStateManager.create("Bob")

        assert(game1 != game2)
        assert(game1.id != game2.id)
      }
    }

    "Starting a game" when {

      "the player is ahead of the enemy" should {
        val gameState = GameStateGenerator.gameStateWithFastPlayer
        val result: GameState = gameStateManager.start(gameState)


        "return with the player's position >= 100" in {
          assert(result.player.position >= 100)
        }
      }
    }
  }
}
