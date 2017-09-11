package chousen.game.actions

import java.util.UUID

import chousen.Optics._
import chousen.api.data._
import chousen.game.core.RandomGameStateCreator
import chousen.game.dungeon.{EnemyBuilder, SimpleDungeonBuilder}
import chousen.game.status.{StatusBuilder, StatusCalculator}
import org.scalatest.WordSpec

class MultiTargetActionHandlerSpec extends WordSpec {

  "MultiTargetActionHandler" when {
    val dungeonBuilder = new SimpleDungeonBuilder()
    val stateCreator = new RandomGameStateCreator(dungeonBuilder)
    val sc = new StatusCalculator
    val damageCalculator = new DamageCalculator(sc)
    val multiTargetActionHandler = new MultiTargetActionHandler(damageCalculator)

    "Given an Action and an UUID that does not match any enemy" should {

      val initialState: GameState = GameStateGenerator.gameStateWithFastPlayer
      val startedGame: GameState = stateCreator.start(initialState)

      val altUUID = UUID.fromString("0709daa1-5975-4f28-b0be-a676f87b70f0")
      lazy val result = multiTargetActionHandler.handle(Set(altUUID), GroundStrike).apply(startedGame)

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

      val targetId = GameStateGenerator.firstEnemy.id
      lazy val result = multiTargetActionHandler.handle(Set(targetId), GroundStrike).apply(startedGame)

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
      val startedGame: GameState = stateCreator.start(gameState)

      val targets = Set(GameStateGenerator.firstEnemy.id, GameStateGenerator.secondEnemy.id)
      lazy val result = multiTargetActionHandler.handle(targets, GroundStrike).apply(startedGame)

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

    "Given Ground Strike" should {
      val (startedGame, result, targets) = completeAction(GroundStrike)

      standardAssertions(startedGame, result, targets)
    }

    "Given WindStrike" should {
      val (startedGame, result, targets) = completeAction(WindStrike)

      standardAssertions(startedGame, result, targets)
    }

    "Given Fireball" should {
      val (startedGame, result, targets) = completeAction(Fireball)

      standardAssertions(startedGame, result, targets)
    }

    "Given Mass Drain" should {
      val (startedGame, result, targets) = completeAction(MassDrain)

      standardAssertions(startedGame, result, targets)
    }

    "Given Shatter" should {
      val (startedGame, result, targets) = completeAction(Shatter)

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
        assert(latestMessages.exists(!_.text.contains("Slime attacks Test Player")))
      }

      "the player's health is reduced" in {
        assert(startedGame.player.stats.currentHp > result.player.stats.currentHp)
      }
    }

    "Given Potion of Flames" should {
      val (startedGame, result, targets) = completeAction(PotionOfFlames)

      "Not Lower the targeted enemies health" in {
        targets.foreach(t => assert(startedGame.dungeon.currentEncounter.enemies.exists(_.id == t)))
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
      }

      "the enemy does not take a turn" in {
        assert(result.player.stats.currentHp == startedGame.player.stats.currentHp)

        assert(latestMessages.exists(!_.text.contains("Slime attacks Test Player")))
      }

      "Gives enemies the burn status" in {
        assert(result.dungeon.currentEncounter.enemies.forall(_.status.exists(_.effect == Burn)))
      }
    }

    "Given Potion of Alkahest" should {
      val (startedGame, result, targets) = completeAction(PotionOfAlkahest)

      "Not Lower the targeted enemies health" in {
        targets.foreach(t => assert(startedGame.dungeon.currentEncounter.enemies.exists(_.id == t)))
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
      }

      "the enemy does not take a turn" in {
        assert(result.player.stats.currentHp == startedGame.player.stats.currentHp)

        assert(latestMessages.exists(!_.text.contains("Slime attacks Test Player")))
      }

      "Gives enemies the poison status" in {
        assert(result.dungeon.currentEncounter.enemies.forall(_.status.exists(_.effect == Poison)))
      }
    }

    "Given Potion of Poison" should {
      val (startedGame, result, targets) = completeAction(PotionOfPoison)

      "Not Lower the targeted enemies health" in {
        targets.foreach(t => assert(startedGame.dungeon.currentEncounter.enemies.exists(_.id == t)))
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
      }

      "the enemy does not take a turn" in {
        assert(result.player.stats.currentHp == startedGame.player.stats.currentHp)

        assert(latestMessages.exists(!_.text.contains("Slime attacks Test Player")))
      }

      "Gives enemies the poison status" in {
        assert(result.dungeon.currentEncounter.enemies.forall(_.status.exists(_.effect == Poison)))
      }
    }


    "Given Potion of Quagmire" should {
      val (startedGame, result, targets) = completeAction(PotionOfQuagmire)

      "Not Lower the targeted enemies health" in {
        targets.foreach(t => assert(startedGame.dungeon.currentEncounter.enemies.exists(_.id == t)))
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
      }

      "the enemy does not take a turn" in {
        assert(result.player.stats.currentHp == startedGame.player.stats.currentHp)

        assert(latestMessages.exists(!_.text.contains("Slime attacks Test Player")))
      }

      "Gives enemies the slow status" in {
        assert(result.dungeon.currentEncounter.enemies.forall(_.status.exists(_.effect == Slow)))
      }
    }

    "Given Potion of Miasma" should {
      val (startedGame, result, targets) = completeAction(PotionOfMiasma)

      "Not Lower the targeted enemies health" in {
        targets.foreach(t => assert(startedGame.dungeon.currentEncounter.enemies.exists(_.id == t)))
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
      }

      "the enemy does not take a turn" in {
        assert(result.player.stats.currentHp == startedGame.player.stats.currentHp)

        assert(latestMessages.exists(!_.text.contains("Slime attacks Test Player")))
      }

      "Gives enemies the burn status" in {
        assert(result.dungeon.currentEncounter.enemies.forall(_.status.exists(_.effect == Burn)))
      }

      "Gives enemies the poison status" in {
        assert(result.dungeon.currentEncounter.enemies.forall(_.status.exists(_.effect == Poison)))
      }

      "Gives enemies the slow status" in {
        assert(result.dungeon.currentEncounter.enemies.forall(_.status.exists(_.effect == Slow)))
      }
    }

    "Given Scroll of Fear" should {
      val (startedGame, result, targets) = completeAction(ScrollOfFear)

      "Not Lower the targeted enemies health" in {
        targets.foreach(t => assert(startedGame.dungeon.currentEncounter.enemies.exists(_.id == t)))
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
      }

      "the enemy does not take a turn" in {
        assert(result.player.stats.currentHp == startedGame.player.stats.currentHp)

        assert(latestMessages.exists(!_.text.contains("Slime attacks Test Player")))
      }

      "Gives enemies the fear status" in {
        assert(result.dungeon.currentEncounter.enemies.forall(_.status.exists(_.effect == Fear)))
      }
    }

    "Given Extinguish" should {
      val (startedGame, result, targets) = completeAction(Extinguish,
        CurrentEnemiesLens.modify(es => es.map(EnemyStatusLens.modify(_ :+ StatusBuilder.makeBurn(1)))))

      "Not Lower the targeted enemies health when all enemies have no status effects" in {
        val (startedGameNoBurn, resultNoBurn, targetsNb) = completeAction(Extinguish)

        targetsNb.foreach(t => assert(startedGameNoBurn.dungeon.currentEncounter.enemies.exists(_.id == t)))
        assert(startedGameNoBurn.dungeon.currentEncounter.enemies.size == 2)
        assert(resultNoBurn.dungeon.currentEncounter.enemies.size == 2)
        assert(getFirstEnemyHp(resultNoBurn) == getFirstEnemyHp(startedGameNoBurn))
        assert(getSecondEnemyHp(resultNoBurn) == getSecondEnemyHp(startedGameNoBurn))
      }

      "the players position is reduced" ignore {
        // Doesn't apply as no enemies are burnt
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

      "Removes the Burn status" in {
        assert(result.dungeon.currentEncounter.enemies.forall(!_.status.exists(_.effect == Burn)))
      }
    }

    "Given Chrysopoeia" should {
      val (startedGame, result, _) = completeAction(Chrysopoeia,
        CurrentEnemiesLens.set(Set(EnemyBuilder.createRat, EnemyBuilder.createSloth,
          EnemyBuilder.knollShaman, EnemyBuilder.orc)))

      "Not Lower the targeted enemies health when all enemies have very high HP" in {
        assert(result.dungeon.currentEncounter.enemies.size == 3)
        assert(result.dungeon.currentEncounter.enemies.head.stats.currentHp ==
          result.dungeon.currentEncounter.enemies.head.stats.maxHp)
        assert(result.dungeon.currentEncounter.enemies.tail.head.stats.currentHp ==
          result.dungeon.currentEncounter.enemies.tail.head.stats.maxHp)
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

      "kills the low hp enemy" in {
        assert(startedGame.dungeon.currentEncounter.enemies.size == 4)
        assert(result.dungeon.currentEncounter.enemies.size == 3)

        assert(result.dungeon.currentEncounter.enemies.size < startedGame.dungeon.currentEncounter.enemies.size )
      }
    }


    def completeAction(action: MultiAction, eff: GameState => GameState= g => g) = {
      val gameState = eff(GameStateGenerator.gameStateWithFastPlayer)
      val startedGame: GameState = stateCreator.start(gameState)

      val targets = gameState.dungeon.currentEncounter.enemies.map(_.id)
      lazy val result = multiTargetActionHandler.handle(targets, action).apply(startedGame)
      (startedGame, result, targets)
    }


    def standardAssertions(startedGame: GameState, result: GameState, targets: Set[UUID]) = {
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
