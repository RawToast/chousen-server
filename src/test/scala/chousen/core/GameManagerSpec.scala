package chousen.core

import api.data.GameMessage
import chousen.character.BaseCharacter
import org.scalatest.{Matchers, WordSpec}

import scala.collection.immutable.Nil

class GameManagerSpec extends WordSpec with Matchers {

  type SingleTargetCommand = (BaseCharacter, Game) => Game
  type MultiTargetCommand = (Set[BaseCharacter], Game) => Game

  "The GameManager" when {

    val gameManager: GameManager = BasicGameManager

    "Creating a game" should {

      val newGame = gameManager.create("Bob")

      "create a game with valid input" in {
        newGame.player.name shouldBe "Bob"
      }

      "contain all initial messages" in {
        newGame.messages.size shouldBe 1
        newGame.messages.head.text shouldBe "Bob has entered the dungeon"
      }
    }

    "Starting a game" should {

      val game = gameManager.create("Bob")

      val startedGame: Game = gameManager.start(game)

      "Result in the player's position being at least" in {
        startedGame.player.position should be >= 100
      }
    }

    "Taking an Attack command" should {

      val game = gameManager.start(gameManager.create("Bob"))
      val enemies = Game.currentEnemies.get(game)
      val target = enemies.head

      val cmd: Command = Command(Set(target), PlayerAttack)

      val result: Game = gameManager.takeCommand(cmd, game)

      "Create a new game with different state" in {
        result shouldNot equal(game)
      }

      "Reduce the target's health" in {
        val enemies = Game.currentEnemies.get(result)

        val newTarget = enemies.find(b => b.id == target.id)

        newTarget shouldNot be(Nil)

        newTarget.get.stats.currentHp should be < newTarget.get.stats.maxHp
      }

      "Update the player's position" in {
        result.player.position shouldNot be(game.player.position)
        result.player.position shouldBe 20
      }

      "Create some messages" ignore {
        result.messages should (be(empty) and (have size 2))

        result.messages.head.text should include("Yellow Slime attacks! Bob takes")
        result.messages.tail.head.text should include("Slime attacks! Bob takes")
      }

      "Player loses health" in {
        result.player.stats.currentHp should be < result.player.stats.maxHp
      }
    }
  }
}

