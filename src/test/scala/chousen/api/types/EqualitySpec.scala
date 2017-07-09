package chousen.api.types

import java.util.UUID

import chousen.api.data.GameStateGenerator
import org.scalatest.WordSpec
import chousen.Implicits._

class EqualitySpec extends WordSpec {

  "Equality" should {

    val player = GameStateGenerator.staticGameState.player
    val enemy = GameStateGenerator.firstEnemy

    "find two objects to be equal" when {
      "both are the same object" in {
        assert(Equality.hasEqualId(player, player))
        assert(Equality.hasEqualId(enemy, enemy))
      }

      "they have the same ids, but other values differ" in {
        assert(Equality.hasEqualId(player, player.copy(position = +1)))
        assert(Equality.hasEqualId(enemy, enemy.copy(position = +1)))
      }
    }

    "find two objects are not equal" when {
      "they have different ids" in {
        assert(!Equality.hasEqualId(enemy,
          enemy.copy(id = UUID.fromString("0709daa1-5975-4f28-b0be-a676f87b70f0") )))
      }
    }

  }
}
