package chousen.game.actions

import java.util.UUID

import chousen.api.data._
import chousen.game.core.GameStateManager
import org.scalatest.WordSpec

class SingleTargetActionHandlerSpec extends WordSpec {

  "SingleTargetActionHandler" when {

    "Given an Action and an UUID that does not match any enemy" should {

      val initialState: GameState = GameStateGenerator.gameStateWithFastPlayer
      val startedGame: GameState = GameStateManager.start(initialState)

      val altUUID = UUID.fromString("0709daa1-5975-4f28-b0be-a676f87b70f0")
      lazy val result = SingleTargetActionHandler.handle(altUUID, QuickAttack).apply(startedGame)

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

      val target = GameStateGenerator.firstEnemy
      lazy val result = SingleTargetActionHandler.handle(target.id, QuickAttack).apply(startedGame)

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
      }

      "the enemy does not take a turn" in {
        assert(result.player.stats.currentHp == startedGame.player.stats.currentHp)

        assert(latestMessages.exists(!_.text.contains("Slime attacks Test Player")))
      }
    }

    "Given Crushing Blow" should {
      val (startedGame, result, target) = completeAction(CrushingBlow)
      standardAssertions(startedGame, result, target)
    }

    "Given Hamstring" should {
      val (startedGame, result, target) = completeAction(Hamstring)
      standardAssertions(startedGame, result, target)

      "Lower the target's speed" in {
        assert(target.stats.speed > result.dungeon.currentEncounter.enemies.find(p => p.id == target.id).map(_.stats.speed).getOrElse(99))
      }
    }

    "Given Stunning Strike" should {
      val (startedGame, result, target) = completeAction(StunningStrike)
      standardAssertions(startedGame, result, target)

      "Lower the target's position" in {
        assert(target.position >= result.dungeon.currentEncounter.enemies.find(p => p.id == target.id).map(_.position).getOrElse(999))
      }
    }

    "Given Counter" should {
      val (startedGame, result, target) = completeAction(Counter)
      standardAssertions(startedGame, result, target)
    }

    "Given Magic Missile" ignore {
      // Will do 0 damage
      val (startedGame, result, target) = completeAction(MagicMissile)
      standardAssertions(startedGame, result, target)
    }

    "Given Triple Strike" should {
      val (startedGame, result, target) = completeAction(TripleStrike)
      standardAssertions(startedGame, result, target)
    }

    "Given Pain" should {
      val (startedGame, result, target) = completeAction(Pain)
      standardAssertions(startedGame, result, target)
    }

    "Given Magic Missile" should {
      val (startedGame, result, target) = completeAction(MagicMissile)
      standardAssertions(startedGame, result, target)
    }

    "Given Drain" should {
      val (startedGame, result, target) = completeAction(Drain)
      standardAssertions(startedGame, result, target)
    }
  }

  private def completeAction(action: SingleTargetAction): (GameState, GameState, Enemy) = {
    val gameState = GameStateGenerator.gameStateWithFastPlayer
    val startedGame: GameState = GameStateManager.start(gameState)

    val target = GameStateGenerator.firstEnemy
    lazy val result = SingleTargetActionHandler.handle(target.id, action).apply(startedGame)
    (startedGame, result, target)
  }

  private def standardAssertions(startedGame: GameState, result: GameState, target: Enemy) = {

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
    }

    "the enemy does not take a turn" in {
      assert(result.player.stats.currentHp == startedGame.player.stats.currentHp)

      assert(latestMessages.exists(!_.text.contains("Slime attacks Test Player")))
    }

  }


  def getFirstEnemyHp(result: GameState) =
    result.dungeon.currentEncounter.enemies
      .find(_.id == GameStateGenerator.firstEnemy.id)
      .map(_.stats.currentHp).getOrElse(404)

}
