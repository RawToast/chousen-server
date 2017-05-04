package chousen.game.actions

import java.util.UUID

import chousen.api.data.{GameMessage, GameState, GameStateGenerator}
import chousen.game.core.GameStateManager
import org.scalatest.WordSpec

object BasicAttackSpec extends WordSpec {

  "Basic Attack" when {

    "Given a UUID that does not match any enemy" should {

      val initialState: GameState = GameStateGenerator.staticGameState
      val altUUID = UUID.fromString("0709daa1-5975-4f28-b0be-a676f87b70f0")
      lazy val result = BasicAttack.attack(altUUID).apply(GameStateGenerator.staticGameState)

      "Have no affect on the player" in {
        assert(result.player == initialState.player)
      }

      "Have no affect on the enemies" in {
        assert(result.dungeon == initialState.dungeon)
      }
    }


    "Given a UUID for an enemy" should {
      val gameState = GameStateGenerator.gameStateWithFastPlayer
      val startedGame: GameState = GameStateManager.start(gameState)

      val target = GameStateGenerator.firstEnemy
      val result = BasicAttack.attack(target.id)(startedGame)

      "Lower the targeted enemies health" in {
        assert(startedGame.dungeon.currentEncounter.enemies.exists(_.id == target.id))
        assert(startedGame.dungeon.currentEncounter.enemies.size == 2)
        assert(result.dungeon.currentEncounter.enemies.size == 2)
        assert(getFirstEnemyHp(result) < getFirstEnemyHp(startedGame))
      }

      "the players position is reduced" in {
        assert(result.player.position < 100)
      }

      lazy val numberOfNewMessages = result.messages.size - startedGame.messages.size
      lazy val latestMessages = result.messages.takeRight(numberOfNewMessages)

      "game messages are created for the player's attack" in {
        assert(result.messages.size > startedGame.messages.size)

        assert(latestMessages.head == GameMessage("Test Player attacks Slime!"))
        assert(latestMessages(1).text.contains("Test Player deals"))
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

}
