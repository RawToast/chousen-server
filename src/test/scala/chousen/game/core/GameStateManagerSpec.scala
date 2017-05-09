package chousen.game.core

import java.util.UUID

import chousen.api.data.{GameStateGenerator, _}
import chousen.game.core.GameStateOptics.DungeonTriLens
import monocle.macros.GenLens
import org.scalatest.WordSpec

class GameStateManagerSpec extends WordSpec {

  "GameStateManager" when {

    val gameStateManager = GameStateManager

    "Creating a game" should {

      "Set the player's name" in {
        val game = gameStateManager.create("Bob")

        assert(game.player.name == "Bob")
      }

      "Set a different ID on each create" in {
        val game1 = gameStateManager.create("Bob")
        val game2 = gameStateManager.create("Bob")

        assert(game1 != game2)
        assert(game1.id != game2.id)
      }
    }

    "Starting a game" when {

      "the player is ahead of the enemy" should {
        val gameState = GameStateGenerator.gameStateWithFastPlayer
        val result: GameState = gameStateManager.start(gameState)


        "return with the player's position >= 100" in {
          assert(result.player.position >= 100)
        }

        "return messages for the start of the game" in {
          assert(result.messages.head == GameMessage(s"${GameStateGenerator.playerName} has entered the dungeon"))
          assert(result.messages(1) == GameMessage(s"${GameStateGenerator.playerName} is attacked by: Slime, Slime!"))
          assert(result.messages(2) == GameMessage(s"${GameStateGenerator.playerName}'s turn!"))
        }

        "return correct message for a single enemy" in {
          val setToSingleEnemy = GenLens[GameState](_.dungeon.currentEncounter.enemies)
            .set(Set(Enemy("Slime", UUID.randomUUID(), CharStats(10, 10), 0)))

          val altResult: GameState = gameStateManager.start(setToSingleEnemy(gameState))

          assert(altResult.messages.head == GameMessage(s"${GameStateGenerator.playerName} has entered the dungeon"))
          assert(altResult.messages(1) == GameMessage(s"${GameStateGenerator.playerName} is attacked by Slime!"))
          assert(altResult.messages(2) == GameMessage(s"${GameStateGenerator.playerName}'s turn!"))
        }
      }
    }

    "Accepting a command" which {
      val gameState = GameStateGenerator.gameStateWithFastPlayer
      val startedGame: GameState = gameStateManager.start(gameState)

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

          import chousen.api.types.Implicits._
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

          import chousen.api.types.Implicits._
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

    "Transitioning a game" should {
      val gameState = GameStateGenerator.staticGameState

      val deadPlayerLens = GameStateOptics.PlayerLens.composeLens(PlayerOptics.PlayerCharStatsLens.composeLens(CharStatsOptics.hp)).set(0)
      val removeEnemies = GameStateOptics.EncounterLens.modify(pem => (pem._1, Set.empty, pem._3))

      "Do nothing if the current encounter is still active" in {
        val result = GameStateManager.transition(gameState)

        assert(result == gameState)
      }

      "Add new messages if the player is dead" in {
        val initialState = deadPlayerLens(gameState)
        val result = GameStateManager.transition(initialState)

        assert(result.id == initialState.id)
        assert(result.player == initialState.player)
        assert(result.dungeon == initialState.dungeon)
        assert(result.messages.size > initialState.messages.size)

      }

      "Transition to the next battle if the encounter is empty" in {
        import chousen.api.types.Implicits._
        val initialState = removeEnemies(gameState)
        val result = GameStateManager.transition(initialState)

        assert(result.id == initialState.id)
        assert(result.player != initialState.player)
        assert(result.player.stats.currentHp <= result.player.stats.maxHp)
        assert(result.player ~= initialState.player)
        assert(result.dungeon != initialState.dungeon)
        assert(result.dungeon.currentEncounter.enemies.nonEmpty)
        assert(result.messages.size > initialState.messages.size)
      }

      "Congradulate the player on victory" in {
        val initialState = DungeonTriLens
          .modify(pdm => (pdm._1, Dungeon(Battle(Set.empty), Seq.empty), pdm._3))(gameState)
        val result = GameStateManager.transition(initialState)

        assert(result.id == initialState.id)
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
