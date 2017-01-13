package chousen.core

import chousen.character.BaseCharacter
import org.scalatest.{Matchers, WordSpec}

class GameManagerSpec extends WordSpec with Matchers {

  type SingleTargetCommand = (BaseCharacter, Game) => Game
  type MultiTargetCommand = (Set[BaseCharacter], Game) => Game

  "The GameManager" when {

    val gameManager:GameManager = ???

    "Creating a game" should {

      "Create new game with valid input" in {

        val newGame = gameManager.create("Bob")

        newGame.playerCharacter.name shouldBe "Bob"
      }
    }

    "Taking a command" should {

      val game = gameManager.create("Bob")
      val cmd: Command = ???

      "Create a new game with different state" in {
        val result: Game = gameManager.takeCommand(game, cmd)
        result shouldNot equal(game)
      }
    }
  }
}
