package chousen.core

import chousen.character.BaseCharacter
import org.scalatest.{Matchers, WordSpec}

class GameManagerSpec extends WordSpec with Matchers {

  type SingleTargetCommand = (BaseCharacter, Game) => Game
  type MultiTargetCommand = (Set[BaseCharacter], Game) => Game

  "The GameManager" when {

    val gameManager:GameManager = BasicGameManager

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

    "Taking a command" should {

      val game = gameManager.create("Bob")

      val encounter = game.quest.current

      val cmd: Command = Command(encounter.enemies, new PlayerAttack())

      "Create a new game with different state" in {
        val result: Game = gameManager.takeCommand(cmd, game)
        result shouldNot equal(game)
      }
    }
  }
}
