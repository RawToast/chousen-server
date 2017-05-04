package chousen.game.core

import java.util.UUID

import chousen.api.data.GameStateGenerator
import org.scalatest.WordSpec


class GameStoreSpec extends WordSpec {

  "GameStore" should {

    "return nothing when given a key that does not exist" in {

      val gs = GameStore
      val result = gs.load(UUID.randomUUID())

      assert(result.isEmpty)
    }

    "be able to save gamestate" in {

      val gs = GameStore

      val result = gs.store(GameStateGenerator.staticGameState)

      assert(result.get(GameStateGenerator.staticGameState.id).isDefined)
    }



    "return something when given a key that does exist" in {

      val gs = GameStore

      gs.store(GameStateGenerator.staticGameState)
      val result = gs.load(GameStateGenerator.staticGameState.id)

      assert(result.isDefined)
    }

  }

}
