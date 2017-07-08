package chousen.api.core

import chousen.api.data.{GameState, GameStateGenerator}
import fs2.Task
import org.http4s.dsl._
import org.scalatest.WordSpec

class MappedGameAccess extends WordSpec {

  //  "MappedGameAccess" should {
  //
  //    val mappedGameAccess = new MappedGameAccess{}
  //
  //    val gameState: GameState = GameStateGenerator.staticGameState
  //
  //    mappedGameAccess.store = mappedGameAccess.store + (GameStateGenerator.uuid -> gameState)
  //
  //    "Return a game with the given id" in {
  //
  //      val result: Output[GameState] = mappedGameAccess.withGame(GameStateGenerator.uuid)(io.finch.Ok)
  //
  //      assert(result.status.code == 200)
  //      val gameState = result.value
  //
  //      assert(gameState.player.name == "Test Player")
  //      assert(gameState.id == GameStateGenerator.uuid)
  //    }
  //
  //    "Return a Finch error when the game does not exist" in {
  //
  //      val uuidString = "a0127253-cdda-48b8-a843-61d450364abf"
  //      val emptyId = java.util.UUID.fromString(uuidString)
  //      val expectedError = s"Game with ID=$uuidString does not exist"
  //
  //      val result: Output[GameState] = mappedGameAccess.withGame(emptyId)(io.finch.Ok)
  //
  //      val exception = intercept[NoSuchElementException] {
  //        result.value
  //      }
  //
  //      assert(exception.getMessage == expectedError)
  //    }
  //
  //  }

  "Http4sMappedGameAccess" should {

    import org.http4s._
    import org.http4s.circe._
    import io.circe.generic.auto._
    import io.circe.syntax._

    val mappedGameAccess = new Http4sMappedGameAccess {}

    val gameState: GameState = GameStateGenerator.staticGameState

    mappedGameAccess.store = mappedGameAccess.store + (GameStateGenerator.uuid -> gameState)

    "Return a game with the given id" in {

      val resultTask: Task[Response] = mappedGameAccess.withGame(GameStateGenerator.uuid)(x => Ok(x.asJson))

      val result = resultTask.unsafeRun()


      assert(result.status.code == 200)
      val gameState = result.as(jsonOf[GameState]).unsafeRun()

      assert(gameState.player.name == "Test Player")
      assert(gameState.id == GameStateGenerator.uuid)
    }

    "Return a 404 response when the game does not exist" in {

      val uuidString = "a0127253-cdda-48b8-a843-61d450364abf"
      val emptyId = java.util.UUID.fromString(uuidString)

      val result = mappedGameAccess.withGame(emptyId)(x => Ok(x.asJson)).unsafeRun()

      assert(result.status.code == 404)
    }

  }
}
