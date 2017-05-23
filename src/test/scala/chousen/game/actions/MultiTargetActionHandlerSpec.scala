package chousen.game.actions

import java.util.UUID

import chousen.api.data.{Fireball, GameState, GameStateGenerator}
import chousen.game.core.GameStateManager
import org.scalatest.WordSpec

class MultiTargetActionHandlerSpec extends WordSpec {

  "MultiTargetActionHandler" when {

    "Given an Action and an UUID that does not match any enemy" should {

      val initialState: GameState = GameStateGenerator.gameStateWithFastPlayer
      val startedGame: GameState = GameStateManager.start(initialState)

      val altUUID = UUID.fromString("0709daa1-5975-4f28-b0be-a676f87b70f0")
      lazy val result = MultiTargetActionHandler.handle(Set(altUUID), Fireball).apply(startedGame)

      "Have no affect on the player" in {
        assert(result.player == startedGame.player)
      }

      "Have no affect on the enemies" in {
        assert(result.dungeon == startedGame.dungeon)
      }
    }


    "Given an Action and a UUID for an enemy" should {
      val gameState = GameStateGenerator.gameStateWithFastPlayer
      val startedGame: GameState = GameStateManager.start(gameState)

      val targetId = GameStateGenerator.firstEnemy.id
      lazy val result = MultiTargetActionHandler.handle(Set(targetId), Fireball).apply(startedGame)

      "Lower the targeted enemies health" in {
        assert(startedGame.dungeon.currentEncounter.enemies.exists(_.id == targetId))
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
      }

      "the enemy does not take a turn" in {
        assert(result.player.stats.currentHp == startedGame.player.stats.currentHp)

        assert(latestMessages.exists(!_.text.contains("Slime attacks Test Player")))
      }
    }

    "Given an Action and a UUIDs for each enemy" should {
      val gameState = GameStateGenerator.gameStateWithFastPlayer
      val startedGame: GameState = GameStateManager.start(gameState)

      val targets = Set(GameStateGenerator.firstEnemy.id, GameStateGenerator.secondEnemy.id)
      lazy val result = MultiTargetActionHandler.handle(targets, Fireball).apply(startedGame)

      "Lower the targeted enemies health" in {
        targets.foreach(t => assert(startedGame.dungeon.currentEncounter.enemies.exists(_.id == t)))
        assert(startedGame.dungeon.currentEncounter.enemies.size == 2)
        assert(result.dungeon.currentEncounter.enemies.size == 2)
        assert(getFirstEnemyHp(result) < getFirstEnemyHp(startedGame))
        assert(getSecondEnemyHp(result) < getSecondEnemyHp(startedGame))
      }

      "the players position is reduced" in {
        assert(result.player.position < 100)
      }

      lazy val numberOfNewMessages = result.messages.size - startedGame.messages.size
      lazy val latestMessages = result.messages.takeRight(numberOfNewMessages)

      "game messages are created for the player's attack" in {
        assert(result.messages.size > startedGame.messages.size)
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
