package chousen.game.actions

import java.util.UUID

import chousen.api.data._
import chousen.game.core.RandomGameStateCreator
import chousen.game.dungeon.SimpleDungeonBuilder
import chousen.game.status.StatusCalculator
import org.scalatest.WordSpec

class SingleTargetActionHandlerSpec extends WordSpec {

  val dungeonBuilder = new SimpleDungeonBuilder()
  val stateCreator = new RandomGameStateCreator(dungeonBuilder)
  val sc = new StatusCalculator
  val dc = new DamageCalculator(sc)
  val singleTargetActionHandler = new SingleTargetActionHandler(dc)

  "SingleTargetActionHandler" when {

    "Given an Action and an UUID that does not match any enemy" should {

      val initialState: GameState = GameStateGenerator.gameStateWithFastPlayer
      val startedGame: GameState = stateCreator.start(initialState)

      val altUUID = UUID.fromString("0709daa1-5975-4f28-b0be-a676f87b70f0")
      lazy val result = singleTargetActionHandler.handle(altUUID, CrushingBlow).apply(startedGame)

      "Have no affect on the player" in {
        assert(result.player == startedGame.player)
      }

      "Have no affect on the enemies" in {
        assert(result.dungeon == startedGame.dungeon)
      }
    }

    "Given an Action and a UUID for an enemy" should {
      val gameState = GameStateGenerator.gameStateWithFastPlayer
      val startedGame: GameState = stateCreator.start(gameState)

      val target = GameStateGenerator.firstEnemy
      lazy val result = singleTargetActionHandler.handle(target.id, CrushingBlow).apply(startedGame)

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

    "Given Stunning Strike" should {
      val (startedGame, result, target) = completeAction(StunningStrike)
      standardAssertions(startedGame, result, target)

      lazy val targetEnemy = result.dungeon.currentEncounter.enemies.find(p => p.id == target.id)

      "Lower the target's position" in {
        // Will not be anywhere near active
        assert(50 >= targetEnemy.map(_.position).getOrElse(999))
      }

      "Apply the Slow status" in {
        assert(targetEnemy.map(_.status).getOrElse(Seq.empty).map(_.effect).contains(Slow))
      }
    }

    "Given Counter" should {
      val (startedGame, result, target) = completeAction(Counter)
      standardAssertions(startedGame, result, target)
    }
  }

  private def completeAction(action: SingleTargetAction): (GameState, GameState, Enemy) = {
    val gameState = GameStateGenerator.gameStateWithFastPlayer
    val startedGame: GameState = stateCreator.start(gameState)

    val target = GameStateGenerator.firstEnemy
    lazy val result = singleTargetActionHandler.handle(target.id, action).apply(startedGame)
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


  private def getFirstEnemyHp(result: GameState) =
    result.dungeon.currentEncounter.enemies
      .find(_.id == GameStateGenerator.firstEnemy.id)
      .map(_.stats.currentHp).getOrElse(404)

}
