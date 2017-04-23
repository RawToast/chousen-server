package chousen.game.api.core

import chousen.api.core.MappedGameAccess
import chousen.api.data.GameState
import chousen.game.core.GameStateManager
import io.finch.Output
import org.scalatest.WordSpec

class MappedGameAccessSpec extends WordSpec {

  "MappedGameAccess" should {

    val mappedGameAccess = new MappedGameAccess{}
    val gameId = java.util.UUID.randomUUID()

    val gameState: GameState = GameStateManager.create("Coolio", gameId)

    mappedGameAccess.store = mappedGameAccess.store + (gameId -> gameState)

    "Return a game with the given id" in {

      val result: Output[GameState] = mappedGameAccess.withGame(gameId)(io.finch.Ok)

      assert(result.status.code == 200)
      val gameState = result.value

      assert(gameState.player.name == "Coolio")
      assert(gameState.id == gameId)
    }

    "Return a Finch error when the game does not exist" in {

      val uuidString = "a0127253-cdda-48b8-a843-61d450364abf"
      val emptyId = java.util.UUID.fromString(uuidString)
      val expectedError = s"Game with ID=$uuidString does not exist"

      val result: Output[GameState] = mappedGameAccess.withGame(emptyId)(io.finch.Ok)

      val exception = intercept[NoSuchElementException] {
        result.value
      }

      assert(exception.getMessage == expectedError)
    }

  }

}
