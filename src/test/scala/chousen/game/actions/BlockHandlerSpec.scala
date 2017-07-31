package chousen.game.actions

import chousen.api.data.{GameMessage, GameState, GameStateGenerator}
import chousen.game.core.RandomGameStateCreator
import chousen.game.dungeon.SimpleDungeonBuilder
import org.scalatest.WordSpec

class BlockHandlerSpec extends WordSpec {

  "BlockActionHandler" when {

    val blockHandler = new BlockActionHandler()

    "Given a UUID for an enemy" should {

      val gameState = GameStateGenerator.gameStateWithFastPlayer
      val dungeonBuilder = new SimpleDungeonBuilder()

      val stateCreator = new RandomGameStateCreator(dungeonBuilder)
      val startedGame: GameState = stateCreator.start(gameState)
      val target = GameStateGenerator.firstEnemy

      val result = blockHandler.block()(startedGame)

      "Not affect any enemies health" in {
        assert(startedGame.dungeon.currentEncounter.enemies.exists(_.id == target.id))
        assert(startedGame.dungeon.currentEncounter.enemies.size == 2)
        assert(result.dungeon.currentEncounter.enemies.size == 2)
        assert(getFirstEnemyHp(result) == getFirstEnemyHp(startedGame))
        assert(getSecondEnemyHp(result) == getSecondEnemyHp(startedGame))
      }

      "the players position is reduced" in {
        assert(result.player.position < 100)
      }

      lazy val numberOfNewMessages = result.messages.size - startedGame.messages.size
      lazy val latestMessages = result.messages.takeRight(numberOfNewMessages)

      "game messages are created for the player's attack" in {
        assert(result.messages.size > startedGame.messages.size)

        assert(latestMessages.head == GameMessage("Test Player blocks"))
      }

      "the enemy does not take a turn" in {
        assert(result.player.stats.currentHp == startedGame.player.stats.currentHp)

        assert(latestMessages.exists(!_.text.contains("Slime attacks Test Player")))
      }
    }
  }


  def getFirstEnemyHp(result: GameState) =
    result.dungeon.currentEncounter.enemies
      .find(_.id == GameStateGenerator.firstEnemy.id)
      .map(_.stats.currentHp).getOrElse(404)

  def getSecondEnemyHp(result: GameState) =
    result.dungeon.currentEncounter.enemies
      .find(_.id == GameStateGenerator.secondEnemy.id)
      .map(_.stats.currentHp).getOrElse(404)
}
