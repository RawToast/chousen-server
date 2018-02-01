package chousen.api.core

import chousen.api.data.GameStateGenerator._
import chousen.api.data.Enemy
import chousen.game.core.GameStateOptics
import org.scalatest.WordSpec

class TargettedLensSpec extends WordSpec {

  "TargettedLens" must {

    "Update a GameState" in {
      val updateState= GameStateOptics.targettedLens(firstEnemy.id)
        // Remove one of the two enemies
        .set(Tuple3(staticGameState.player, Option.empty[Enemy], staticGameState.messages))

      val newGameState = updateState(staticGameState)

      assert(staticGameState.dungeon.currentEncounter.enemies.size == 2)
      assert(newGameState.dungeon.currentEncounter.enemies.size == 1)
    }

  }

}
