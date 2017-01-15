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

    "Loading a game" should {

      val game = gameManager.create("Bob")

      "Load an existing game, in the same state" in {

        GameStore.store(game)
        val sameGameOption:Option[Game] = GameStore.load(game.id)

        sameGameOption shouldNot be(empty)
        val sameGame = sameGameOption.get


        game.id shouldBe sameGame.id
        game.player.name shouldBe sameGame.player.name
        game shouldBe sameGame
      }
    }
  }
}
