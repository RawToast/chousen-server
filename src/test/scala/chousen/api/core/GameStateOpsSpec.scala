package chousen.api.core

import chousen.api.data.GameStateGenerator._
import chousen.api.data.Enemy
import chousen.game.core.GameStateOps
import org.scalatest.WordSpec

class GameStateOpsSpec extends WordSpec {

  "TargettedLens" must {

    "Update a GameState" in {
      val updateState= GameStateOps.targettedLens(firstEnemy.id)
        // Remove one of the two enemies
        .set(Tuple3(staticGameState.player, Option.empty[Enemy], staticGameState.messages))

      val newGameState = updateState(staticGameState)

      assert(staticGameState.dungeon.currentEncounter.enemies.size == 2)
      assert(newGameState.dungeon.currentEncounter.enemies.size == 1)
    }

  }

}
