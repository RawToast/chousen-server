package chousen.game.actions

import chousen.api.data.{GameMessage, GameState, GameStateGenerator, HealWounds}
import chousen.game.core.RandomGameStateCreator
import chousen.game.dungeon.SimpleDungeonBuilder
import org.scalatest.WordSpec

class SelfActionHandlerSpec extends WordSpec {

  "Self Targeting Action Handler" when {

    "Given a self targeting action" should {
      val gameState = GameStateGenerator.gameStateWithFastPlayer

      val dungeonBuilder = new SimpleDungeonBuilder()
      val stateCreator = new RandomGameStateCreator(dungeonBuilder)
      val startedGame: GameState = stateCreator.start(gameState)

      val result = SelfActionHandler.handle(HealWounds)(startedGame)

      lazy val numberOfNewMessages = result.messages.size - startedGame.messages.size
      lazy val latestMessages = result.messages.takeRight(numberOfNewMessages)

      "State the action was used" in {
        assert(result.messages.size > startedGame.messages.size)
        assert(result.messages.contains(GameMessage(s"${GameStateGenerator.playerName} uses Heal Wounds and recovers 30HP!")))
      }

      "Reduce the player's position" in {
        assert(result.player.position < 100)
      }

      "the enemy does not take a turn" in {
        assert(result.player.stats.currentHp == startedGame.player.stats.currentHp)

        assert(latestMessages.exists(!_.text.contains("Slime attacks Test Player")))
      }
    }
  }
}
