package chousen.game.core

import java.util.UUID

import chousen.api.data.{GameStateGenerator, _}
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
          assert(result.messages(1) == GameMessage(s"${GameStateGenerator.playerName} encounters: Slime, Slime"))
          assert(result.messages(2) == GameMessage(s"${GameStateGenerator.playerName}'s turn!"))
        }

        "return correct message for a single enemy" in {
          val setToSingleEnemy = GenLens[GameState](_.dungeon.currentEncounter.enemies)
            .set(Set(Enemy("Slime", UUID.randomUUID(), CharStats(10, 10), 0)))

          val altResult: GameState = gameStateManager.start(setToSingleEnemy(gameState))

          assert(altResult.messages.head == GameMessage(s"${GameStateGenerator.playerName} has entered the dungeon"))
          assert(altResult.messages(1) == GameMessage(s"${GameStateGenerator.playerName} encounters a Slime!"))
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

          assert(latestMessages.head == GameMessage("Test Player attacks Slime!"))
          assert(latestMessages(1).text.contains("Test Player deals"))
        }

        "the enemy takes their turn" in {
          assert(result.player.stats.currentHp < startedGame.player.stats.currentHp)

          assert(latestMessages.exists(_.text.contains("Slime attacks Test Player")))
          assert(latestMessages.exists(_.text.contains("Slime deals")))
        }
      }
    }
  }

  def getFirstEnemyHp(result: GameState) =
    result.dungeon.currentEncounter.enemies
      .find(_.id == GameStateGenerator.firstEnemy.id)
      .map(_.stats.currentHp).getOrElse(404)
}
