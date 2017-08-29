package chousen.game.core

import java.util.UUID

import chousen.api.data.{GameStateGenerator, _}
import chousen.game.dungeon.SimpleDungeonBuilder
import monocle.macros.GenLens
import org.scalatest.WordSpec

class GameStateCreationSpec extends WordSpec {

  "GameStateCreation" when {
    val dungeonBuilder = new SimpleDungeonBuilder()

    val creator = new RandomGameStateCreator(dungeonBuilder)

    "Creating a game" should {

      "Set the player's name" in {
        val game = creator.create("Bob")

        assert(game.player.name == "Bob")
      }

      "Set a different ID on each create" in {
        val game1 = creator.create("Bob")
        val game2 = creator.create("Bob")

        assert(game1 != game2)
        assert(game1.uuid != game2.uuid)

        assert(game1.player.className == game2.player.className)
      }

      "Set a different class with a different choice value" in {
        val game1 = creator.create("Bob", 1)
        val game2 = creator.create("Bob", 2)
        val game3 = creator.create("Bob", 3)
        val game4 = creator.create("Bob", 4)
        val game5 = creator.create("Bob", 5)
        val game6 = creator.create("Bob", 6)


        assert(game1 != game2)
        assert(game1.uuid != game2.uuid)
        val gc1 = game1.player.className
        val gc2 = game2.player.className
        val gc3 = game3.player.className
        val gc4 = game4.player.className
        val gc5 = game5.player.className
        val gc6 = game6.player.className

        assert(Set(gc1, gc2, gc3, gc4, gc5, gc6).size == 6)
        }
    }

    "Starting a game" when {

      "the player is ahead of the enemy" should {
        val gameState = GameStateGenerator.gameStateWithFastPlayer
        val result: GameState = creator.start(gameState)


        "return with the player's position >= 100" in {
          assert(result.player.position >= 100)
        }

        "return messages for the start of the game" in {
          assert(result.messages.head == GameMessage(s"${GameStateGenerator.playerName} has entered the dungeon"))
          assert(result.messages(1) == GameMessage(s"${GameStateGenerator.playerName} is attacked by: Slime, Slime!"))
          assert(result.messages(2) == GameMessage(s"${GameStateGenerator.playerName}'s turn."))
        }

        "return correct message for a single enemy" in {
          val setToSingleEnemy = GenLens[GameState](_.dungeon.currentEncounter.enemies)
            .set(Set(Enemy("Slime", UUID.randomUUID(), CharStats(10, 10), 0)))

          val altResult: GameState = creator.start(setToSingleEnemy(gameState))

          assert(altResult.messages.head == GameMessage(s"${GameStateGenerator.playerName} has entered the dungeon"))
          assert(altResult.messages(1) == GameMessage(s"${GameStateGenerator.playerName} is attacked by Slime!"))
          assert(altResult.messages(2) == GameMessage(s"${GameStateGenerator.playerName}'s turn."))
        }
      }
    }
  }
}
