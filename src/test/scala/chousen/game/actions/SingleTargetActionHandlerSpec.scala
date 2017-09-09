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
      val result = singleTargetActionHandler.handle(altUUID, CrushingBlow).apply(startedGame)

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
      val result = singleTargetActionHandler.handle(target.id, CrushingBlow).apply(startedGame)

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

    "Given Destruction" should {
      val (startedGame, result, target) = completeAction(Destruction)
      standardAssertions(startedGame, result, target)

      "Lower the targets vitality" in {
        lazy val targetEnemy = result.dungeon.currentEncounter.enemies.find(p => p.id == target.id)

        assert(target.stats.vitality > targetEnemy.map(_.stats.vitality).getOrElse(999))
      }
    }

    "Given Burning Hammer" should {
      val (startedGame, result, target) = completeAction(BurningHammer)
      standardAssertions(startedGame, result, target)

      "Apply the Burn status" in {
        lazy val targetEnemy = result.dungeon.currentEncounter.enemies.find(p => p.id == target.id)

        assert(targetEnemy.map(_.status).getOrElse(Seq()).exists(_.effect == Burn))
      }
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

    "Given Ember" should {
      val (startedGame, result, target) = completeAction(Ember)
      standardAssertions(startedGame, result, target)

      "Apply the Burn status" in {
        lazy val targetEnemy = result.dungeon.currentEncounter.enemies.find(p => p.id == target.id)

        assert(targetEnemy.map(_.status).getOrElse(Seq()).exists(_.effect == Burn))
      }
    }


    "Given Quick Attack" should {
      val (startedGame, result, target) = completeAction(QuickAttack)
      standardAssertions(startedGame, result, target)
    }

    "Given Assassinate Attack" should {
      val (startedGame, result, target) = completeAction(Assassinate)
      standardAssertions(startedGame, result, target)
    }

    "Given Toxic Shiv" should {
      val (startedGame, result, target) = completeAction(ToxicShiv)
      standardAssertions(startedGame, result, target)

      "Apply the Poison status" in {
        lazy val targetEnemy = result.dungeon.currentEncounter.enemies.find(p => p.id == target.id)

        assert(targetEnemy.map(_.status).getOrElse(Seq()).exists(_.effect == Poison))
      }

      "Apply the Slow status" in {
        lazy val targetEnemy = result.dungeon.currentEncounter.enemies.find(p => p.id == target.id)

        assert(targetEnemy.map(_.status).getOrElse(Seq()).exists(_.effect == Slow))
      }
    }

    "Given Mammonite" should {
      val (startedGame, result, target) = completeAction(Mammonite)
      standardAssertions(startedGame, result, target)

      "Reduce the player's gold" in {
        startedGame.player.gold > result.player.gold
      }
    }

    "Given Bankruptcy" should {
      val (startedGame, result, target) = completeAction(Bankruptcy)
      standardAssertions(startedGame, result, target)

      "Reduce the player's gold" in {
        startedGame.player.gold > result.player.gold
      }
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
