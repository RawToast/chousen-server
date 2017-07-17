package chousen.game.core

import java.util.UUID

import chousen.api.data.{GameStateGenerator, _}
import chousen.game.core.GameStateOptics.DungeonTriLens
import chousen.game.dungeon.SimpleDungeonBuilder
import org.scalatest.WordSpec

class GameStateManagerSpec extends WordSpec {

  "GameStateManager" when {

    val gameStateManager = new GameStateManager()
    val dungeonBuilder = new SimpleDungeonBuilder()
    val gameStateCreator = new RandomGameStateCreator(dungeonBuilder)
    val gameState = GameStateGenerator.gameStateWithFastPlayer
    val startedGame: GameState = gameStateCreator.start(gameState)

    "Accepting a command" which {
      "Is a basic attack" should {
        val target = GameStateGenerator.firstEnemy
        val result = gameStateManager.takeCommand(AttackRequest(target.id), startedGame)

        "Lower the targeted enemies health" in {
          assert(startedGame.dungeon.currentEncounter.enemies.exists(_.id == target.id))
          assert(startedGame.dungeon.currentEncounter.enemies.size == 2)
          assert(result.dungeon.currentEncounter.enemies.size == 2)
          assert(getFirstEnemyHp(result) < getFirstEnemyHp(startedGame))
        }

        "the player is set back to active" in {
          assert(result.player.position > 100)
          val active = EncounterOps.getActive((result.player,
            result.dungeon.currentEncounter.enemies, result.messages))
          assert(active.isLeft)

          import chousen.Implicits._
          active.swap.foreach(_ ~= startedGame.player)
        }

        lazy val numberOfNewMessages = result.messages.size - startedGame.messages.size
        lazy val latestMessages = result.messages.takeRight(numberOfNewMessages)

        "game messages are created for the player's attack" in {
          assert(result.messages.size > startedGame.messages.size)

          assert(latestMessages.head == GameMessage("Test Player attacks Slime."))
          assert(latestMessages(1).text.contains("Test Player's attack deals"))
        }

        "the enemy takes their turn" in {
          assert(result.player.stats.currentHp < startedGame.player.stats.currentHp)

          assert(latestMessages.exists(_.text.contains("Slime attacks Test Player")))
          assert(latestMessages.exists(_.text.contains(" damage.")))
        }
      }


      "Is a basic single target command" should {
        val target = GameStateGenerator.firstEnemy
        val singleTargetCommand =  SingleTargetActionRequest(target.id, CrushingBlow)
        val result = gameStateManager.takeCommand(singleTargetCommand, startedGame)

        "Lower the targeted enemies health" in {
          assert(startedGame.dungeon.currentEncounter.enemies.exists(_.id == target.id))
          assert(startedGame.dungeon.currentEncounter.enemies.size == 2)
          assert(result.dungeon.currentEncounter.enemies.size == 2)
          assert(getFirstEnemyHp(result) < getFirstEnemyHp(startedGame))
        }

        "the player is set back to active" in {
          assert(result.player.position > 100)
          val active = EncounterOps.getActive((result.player,
            result.dungeon.currentEncounter.enemies, result.messages))
          assert(active.isLeft)

          import chousen.Implicits._
          active.swap.foreach(_ ~= startedGame.player)
        }

        lazy val numberOfNewMessages = result.messages.size - startedGame.messages.size
        lazy val latestMessages = result.messages.takeRight(numberOfNewMessages)

        "game messages are created for the player's attack" in {
          assert(result.messages.size > startedGame.messages.size)

          assert(latestMessages.head == GameMessage("Test Player jumps in the air and lands a crushing blow to Slime!"))
          assert(latestMessages(1).text.contains("Slime takes"))
        }

        "the enemy takes their turn" in {
          assert(result.player.stats.currentHp < startedGame.player.stats.currentHp)

          assert(latestMessages.exists(_.text.contains("Slime attacks Test Player")))
          assert(latestMessages.exists(_.text.contains(" damage.")))
        }
      }
    }

    "Accepting a card" which {

      "The user does not have" should {

        "Return the game state with no changes" in {
          val anotherCard = GameStateGenerator.crushingBlowCard.copy(id = UUID.fromString("221c878f-5a6f-4276-a52e-862cfa90e114"))

          val request = SingleTargetActionRequest(GameStateGenerator.firstEnemy.id, CrushingBlow)

          val result = gameStateManager.useCard(anotherCard, request, gameState)

          assert(result == gameState)
        }
      }

      "Is different to the specified action" should {

        "Return the game state with no changes" in {
          lazy val incorrectCard = GameStateGenerator.quickAttackCard
          val request = SingleTargetActionRequest(GameStateGenerator.firstEnemy.id, CrushingBlow)

          val result = gameStateManager.useCard(incorrectCard, request, gameState)

          assert(result == gameState)
        }
      }

      "Is a valid single target request" should {

        lazy val card = GameStateGenerator.crushingBlowCard

        val initialState = GameStateOptics.HandLens.modify(_ :+ card)(gameState)

        val request = SingleTargetActionRequest(GameStateGenerator.firstEnemy.id, CrushingBlow)

        lazy val result = gameStateManager.useCard(card, request, initialState)

        "Change the game state" in {
          assert(result != initialState)
        }

        "Remove the card from the player's hand" in {
          //assert(initialState.cards.hand.size > result.cards.hand.size)
          assert(!result.cards.hand.contains(card))
        }
      }
    }

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

  def getFirstEnemyHp(result: GameState) =
    result.dungeon.currentEncounter.enemies
      .find(_.id == GameStateGenerator.firstEnemy.id)
      .map(_.stats.currentHp).getOrElse(404)
}
