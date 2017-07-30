package chousen.game.actions

import chousen.api.data._
import chousen.game.core.RandomGameStateCreator
import chousen.game.dungeon.SimpleDungeonBuilder
import org.scalatest.WordSpec

class SelfActionHandlerSpec extends WordSpec {

  "Self Targeting Action Handler" when {

    "Given a self targeting action" should {
      val gameState = GameStateGenerator.gameStateWithFastPlayer

      val dungeonBuilder = new SimpleDungeonBuilder()
      val stateCreator = new RandomGameStateCreator(dungeonBuilder)
      val startedGame: GameState = stateCreator.start(gameState)

      val result = SelfActionHandler.handle(HealWounds)(startedGame)

      lazy val numberOfNewMessages = result.messages.size - startedGame.messages.size
      lazy val latestMessages = result.messages.takeRight(numberOfNewMessages)

      "State the action was used" in {
        assert(result.messages.size > startedGame.messages.size)
        assert(result.messages.contains(GameMessage(s"${GameStateGenerator.playerName} uses Heal Wounds and recovers 30HP!")))
      }

      "Reduce the player's position" in {
        assert(result.player.position < 100)
      }

      "the enemy does not take a turn" in {
        assert(result.player.stats.currentHp == startedGame.player.stats.currentHp)

        assert(latestMessages.exists(!_.text.contains("Slime attacks Test Player")))
      }
    }

    "Given a Haste action" should {
      val gameState = GameStateGenerator.gameStateWithFastPlayer

      val dungeonBuilder = new SimpleDungeonBuilder()
      val stateCreator = new RandomGameStateCreator(dungeonBuilder)
      val startedGame: GameState = stateCreator.start(gameState)

      val result = SelfActionHandler.handle(Haste)(startedGame)

      "State the action was used" in {
        assert(result.messages.size > startedGame.messages.size)
        assert(result.messages.contains(GameMessage(s"${GameStateGenerator.playerName} uses Haste!")))
      }

      "Reduce the player's position" in {
        assert(result.player.position < 100)
      }

      "The Player gains the Haste status" in {
        assert(result.player.status.nonEmpty)
        assert(result.player.status.exists(_.effect == Fast))
      }
    }

    "Given a Might action" should {
      val gameState = GameStateGenerator.gameStateWithFastPlayer

      val dungeonBuilder = new SimpleDungeonBuilder()
      val stateCreator = new RandomGameStateCreator(dungeonBuilder)
      val startedGame: GameState = stateCreator.start(gameState)

      val result = SelfActionHandler.handle(PotionOfMight)(startedGame)

      "State the action was used" in {
        assert(result.messages.size > startedGame.messages.size)
        assert(result.messages.contains(GameMessage(s"${GameStateGenerator.playerName} drinks a Potion of Might!")))
      }

      "Reduce the player's position" in {
        assert(result.player.position < 100)
      }

      "The Player gains the Might status" in {
        assert(result.player.status.nonEmpty)
        assert(result.player.status.exists(_.effect == Might))
      }
    }

    "Given a Potion of Dexterity action" should {
      val gameState = GameStateGenerator.gameStateWithFastPlayer

      val dungeonBuilder = new SimpleDungeonBuilder()
      val stateCreator = new RandomGameStateCreator(dungeonBuilder)
      val startedGame: GameState = stateCreator.start(gameState)

      val result = SelfActionHandler.handle(PotionOfDexterity)(startedGame)

      "State the action was used" in {
        assert(result.messages.size > startedGame.messages.size)
        assert(result.messages.contains(GameMessage(s"${GameStateGenerator.playerName} drinks a Potion of Dexterity!")))
      }

      "Reduce the player's position" in {
        assert(result.player.position < 100)
      }

      "The Player gains the Dexterity status" in {
        assert(result.player.status.nonEmpty)
        assert(result.player.status.exists(_.effect == Dexterity))
      }
    }

    "Given a Potion of Intelligence action" should {
      val gameState = GameStateGenerator.gameStateWithFastPlayer

      val dungeonBuilder = new SimpleDungeonBuilder()
      val stateCreator = new RandomGameStateCreator(dungeonBuilder)
      val startedGame: GameState = stateCreator.start(gameState)

      val result = SelfActionHandler.handle(PotionOfIntelligence)(startedGame)

      "State the action was used" in {
        assert(result.messages.size > startedGame.messages.size)
        assert(result.messages.contains(GameMessage(s"${GameStateGenerator.playerName} drinks a Potion of Intelligence!")))
      }

      "Reduce the player's position" in {
        assert(result.player.position < 100)
      }

      "The Player gains the Smart status" in {
        assert(result.player.status.nonEmpty)
        assert(result.player.status.exists(_.effect == Smart))
      }
    }

    "Given a Potion of Stone Skin action" should {
      val gameState = GameStateGenerator.gameStateWithFastPlayer

      val dungeonBuilder = new SimpleDungeonBuilder()
      val stateCreator = new RandomGameStateCreator(dungeonBuilder)
      val startedGame: GameState = stateCreator.start(gameState)

      val result = SelfActionHandler.handle(PotionOfStoneSkin)(startedGame)

      "State the action was used" in {
        assert(result.messages.size > startedGame.messages.size)
        assert(result.messages.contains(GameMessage(s"${GameStateGenerator.playerName} drinks a Potion of Stone Skin!")))
      }

      "Reduce the player's position" in {
        assert(result.player.position < 100)
      }

      "The Player gains the StoneSkin status" in {
        assert(result.player.status.nonEmpty)
        assert(result.player.status.exists(_.effect == StoneSkin))
      }
    }
  }
}
