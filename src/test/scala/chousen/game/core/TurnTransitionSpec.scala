package chousen.game.core

import chousen.Optics._
import chousen.api.data._
import chousen.game.status.{PostTurnStatusCalculator, StatusBuilder}
import org.scalatest.WordSpec

class TurnTransitionSpec extends WordSpec {

  "TurnTransition" when {

    val turnTransition = new TurnTransition {}
    val postTurnStatusCalc = new PostTurnStatusCalculator

    "Transitioning a game" should {
      val gameState = GameStateGenerator.staticGameState

      val deadPlayerLens = (PlayerLens ^|-> (PlayerCharStatsLens ^|-> HpLens)).set(0)
      val removeEnemies = EncounterLens.modify(pem => (pem._1, Set.empty, pem._3))

      "Do nothing if the current encounter is still active" in {
        val result = turnTransition.transitionGame(gameState, postTurnStatusCalc)

        assert(result == gameState)
      }

      "Add new messages if the player is dead" in {
        val initialState = deadPlayerLens(gameState)
        val result = turnTransition.transitionGame(initialState, postTurnStatusCalc)

        assert(result.uuid == initialState.uuid)
        assert(result.player == initialState.player)
        assert(result.dungeon == initialState.dungeon)
        assert(result.messages.size > initialState.messages.size)

      }

      "Transition to the next battle if the encounter is empty" in {
        import chousen.Implicits._
        val initialState = removeEnemies(gameState)
        val result = turnTransition.transitionGame(initialState, postTurnStatusCalc)


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
        val result = turnTransition.transitionGame(initialState, postTurnStatusCalc)

        assert(result.uuid == initialState.uuid)
        assert(result.player == initialState.player)
        assert(result.dungeon == initialState.dungeon)
        assert(result.dungeon.currentEncounter.enemies.isEmpty)
        assert(result.messages.size > initialState.messages.size)
        assert(result.messages.last.text.contains("win"))
      }

      "Reduce the length of any Status effects" in {
        val preTransitionGameState = (PlayerLens ^|-> PlayerStatusLens)
          .set(Seq(StatusBuilder.makeMight(4)))(gameState)

        val result = turnTransition.transitionGame(preTransitionGameState, postTurnStatusCalc)

        assert(result.player.status.head.turns < preTransitionGameState.player.status.head.turns)
      }


      "Remove any expired Status effects" in {
        val preTransitionGameState = (PlayerLens ^|-> PlayerStatusLens)
          .set(Seq(StatusBuilder.makeMight(4, turns = 0)))(gameState)

        val result = turnTransition.transitionGame(preTransitionGameState, postTurnStatusCalc)

        assert(result.player.status.isEmpty)
      }

    }
  }
}
