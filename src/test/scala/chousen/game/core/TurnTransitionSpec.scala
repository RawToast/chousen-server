package chousen.game.core

import chousen.api.data._
import chousen.game.core.GameStateOptics.DungeonTriLens
import org.scalatest.WordSpec

class TurnTransitionSpec extends WordSpec{

  "Transition" when {

    val gameStateManager = new TurnTransition {}

    "Transitioning a game" should {
      val gameState = GameStateGenerator.staticGameState

      val deadPlayerLens = GameStateOptics.PlayerLens.composeLens(PlayerOptics.PlayerCharStatsLens.composeLens(CharStatsOptics.HpLens)).set(0)
      val removeEnemies = GameStateOptics.EncounterLens.modify(pem => (pem._1, Set.empty, pem._3))

      "Do nothing if the current encounter is still active" in {
        val result = gameStateManager.transition(gameState)

        assert(result == gameState)
      }

      "Add new messages if the player is dead" in {
        val initialState = deadPlayerLens(gameState)
        val result = gameStateManager.transition(initialState)

        assert(result.uuid == initialState.uuid)
        assert(result.player == initialState.player)
        assert(result.dungeon == initialState.dungeon)
        assert(result.messages.size > initialState.messages.size)

      }

      "Transition to the next battle if the encounter is empty" in {
        import chousen.Implicits._
        val initialState = removeEnemies(gameState)
        val result = gameStateManager.transition(initialState)


        assert(result.uuid == initialState.uuid)
        assert(result.player != initialState.player)
        assert(result.player.stats.currentHp <= result.player.stats.maxHp)
        assert(result.player ~= initialState.player)
        assert(result.dungeon != initialState.dungeon)
        assert(result.dungeon.currentEncounter.enemies.nonEmpty)
        assert(result.messages.size > initialState.messages.size)
      }

      "Congratulate the player on victory" in {
        val initialState = DungeonTriLens
          .modify(pdm => (pdm._1, Dungeon(Battle(Set.empty), Seq.empty), pdm._3))(gameState)
        val result = gameStateManager.transition(initialState)

        assert(result.uuid == initialState.uuid)
        assert(result.player == initialState.player)
        assert(result.dungeon == initialState.dungeon)
        assert(result.dungeon.currentEncounter.enemies.isEmpty)
        assert(result.messages.size > initialState.messages.size)
        assert(result.messages.last.text.contains("win"))
      }

    }
  }
}
