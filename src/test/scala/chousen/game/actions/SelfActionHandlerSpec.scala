package chousen.game.actions

import chousen.api.data._
import chousen.game.core.RandomGameStateCreator
import chousen.game.dungeon.SimpleDungeonBuilder
import chousen.game.status.StatusCalculator
import monocle.macros.GenLens
import org.scalatest.WordSpec

class SelfActionHandlerSpec extends WordSpec {

  "Self Targeting Action Handler" when {

    val sc = new StatusCalculator
    val selfActionHandler = new SelfActionHandler(sc)

    "Given a self targeting action" should {
      val gameState = GameStateGenerator.gameStateWithFastPlayer

      val dungeonBuilder = new SimpleDungeonBuilder()
      val stateCreator = new RandomGameStateCreator(dungeonBuilder)
      val startedGame: GameState = stateCreator.start(gameState)

      val result = selfActionHandler.handle(HealWounds)(startedGame)

      lazy val numberOfNewMessages = result.messages.size - startedGame.messages.size
      lazy val latestMessages = result.messages.takeRight(numberOfNewMessages)

      "State the action was used" in {
        assert(result.messages.size > startedGame.messages.size)
        assert(result.messages.map(_.text).exists(_.contains(s"${GameStateGenerator.playerName} uses Heal Wounds and recovers")))
      }

      "Reduce the player's position" in {
        assert(result.player.position < 100)
      }

      "the enemy does not take a turn" in {
        assert(result.player.stats.currentHp == startedGame.player.stats.currentHp)

        assert(latestMessages.exists(!_.text.contains("Slime attacks Test Player")))
      }
    }

    "Given a Strength Elixir" should {
      val gameState = GameStateGenerator.gameStateWithFastPlayer

      val dungeonBuilder = new SimpleDungeonBuilder()
      val stateCreator = new RandomGameStateCreator(dungeonBuilder)
      val startedGame: GameState = stateCreator.start(gameState)

      val result = selfActionHandler.handle(ElixirOfStrength)(startedGame)

      "State the action was used" in {
        assert(result.messages.size > startedGame.messages.size)
      }

      "Reduce the player's position" in {
        assert(result.player.position < 100)
      }

      "The Player's strength increases" in {
        assert(result.player.stats.strength > startedGame.player.stats.strength)
      }
    }

    "Given a Dexterity Elixir" should {
      val gameState = GameStateGenerator.gameStateWithFastPlayer

      val dungeonBuilder = new SimpleDungeonBuilder()
      val stateCreator = new RandomGameStateCreator(dungeonBuilder)
      val startedGame: GameState = stateCreator.start(gameState)

      val result = selfActionHandler.handle(ElixirOfDexterity)(startedGame)

      "State the action was used" in {
        assert(result.messages.size > startedGame.messages.size)
      }

      "Reduce the player's position" in {
        assert(result.player.position < 100)
      }

      "The Player's dexterity increases" in {
        assert(result.player.stats.dexterity > startedGame.player.stats.dexterity)
      }
    }


    "Given an Elixir of Intellegence" should {
      val gameState = GameStateGenerator.gameStateWithFastPlayer

      val dungeonBuilder = new SimpleDungeonBuilder()
      val stateCreator = new RandomGameStateCreator(dungeonBuilder)
      val startedGame: GameState = stateCreator.start(gameState)

      val result = selfActionHandler.handle(ElixirOfIntelligence)(startedGame)

      "State the action was used" in {
        assert(result.messages.size > startedGame.messages.size)
      }

      "Reduce the player's position" in {
        assert(result.player.position < 100)
      }

      "The Player's intellect increases" in {
        assert(result.player.stats.intellect > startedGame.player.stats.intellect)
      }
    }

    "Given an Elixir of Vitality" should {
      val gameState = GameStateGenerator.gameStateWithFastPlayer

      val dungeonBuilder = new SimpleDungeonBuilder()
      val stateCreator = new RandomGameStateCreator(dungeonBuilder)
      val startedGame: GameState = stateCreator.start(gameState)

      val result = selfActionHandler.handle(ElixirOfVitality)(startedGame)

      "State the action was used" in {
        assert(result.messages.size > startedGame.messages.size)
      }

      "Reduce the player's position" in {
        assert(result.player.position < 100)
      }

      "The Player's vitality increases" in {
        assert(result.player.stats.vitality > startedGame.player.stats.vitality)
      }
    }

    "Given a Rare Pepe" should {
      val gameState = GameStateGenerator.gameStateWithFastPlayer

      val dungeonBuilder = new SimpleDungeonBuilder()
      val stateCreator = new RandomGameStateCreator(dungeonBuilder)
      val startedGame: GameState = stateCreator.start(gameState)

      val result = selfActionHandler.handle(RarePepe)(startedGame)

      "State the action was used" in {
        assert(result.messages.size > startedGame.messages.size)
      }

      "Reduce the player's position" in {
        assert(result.player.position < 100)
      }

      "The Player gains experience" in {
        assert(result.player.experience != startedGame.player.experience)
      }
    }

    "Given a Haste action" should {
      val gameState = GameStateGenerator.gameStateWithFastPlayer

      val dungeonBuilder = new SimpleDungeonBuilder()
      val stateCreator = new RandomGameStateCreator(dungeonBuilder)
      val startedGame: GameState = stateCreator.start(gameState)

      val result = selfActionHandler.handle(Haste)(startedGame)

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

      val result = selfActionHandler.handle(PotionOfMight)(startedGame)

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

      val result = selfActionHandler.handle(PotionOfDexterity)(startedGame)

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

      val result = selfActionHandler.handle(PotionOfIntelligence)(startedGame)

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

      val result = selfActionHandler.handle(PotionOfStoneSkin)(startedGame)

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

    "Given Potion of Rage" should {
      val gameState = GameStateGenerator.gameStateWithFastPlayer

      val dungeonBuilder = new SimpleDungeonBuilder()
      val stateCreator = new RandomGameStateCreator(dungeonBuilder)
      val startedGame: GameState = stateCreator.start(gameState)

      val result = selfActionHandler.handle(PotionOfRage)(startedGame)

      "State the action was used" in {
        assert(result.messages.size > startedGame.messages.size)
        assert(result.messages.contains(GameMessage(s"${GameStateGenerator.playerName} drinks a Potion of Rage!")))
      }

      "Reduce the player's position" in {
        assert(result.player.position < 100)
      }

      "The Player gains the Berserk status" in {
        assert(result.player.status.nonEmpty)
        assert(result.player.status.exists(_.effect == Rage))
      }
    }

    "Given Potion of Continuation" should {
      val gameState = GameStateGenerator.gameStateWithFastPlayer

      val dungeonBuilder = new SimpleDungeonBuilder()
      val stateCreator = new RandomGameStateCreator(dungeonBuilder)
      val startedGame: GameState = stateCreator.start(gameState)

      val initialState = GenLens[GameState](_.player.status)
        .set(Seq(Status(Rage, "test", 3), Status(Fast, "test", 3)))
        .apply(startedGame)

      val result = selfActionHandler.handle(PotionOfContinuation)(initialState)

      "State the action was used" in {
        assert(result.messages.size > initialState.messages.size)
        assert(result.messages.contains(GameMessage(s"${GameStateGenerator.playerName} drinks a Potion of Continuation!")))
      }

      "Reduce the player's position" in {
        assert(result.player.position < 100)
      }

      "The Player's status effects length has increased" in {
        assert(initialState.player.status.nonEmpty)
        assert(result.player.status.nonEmpty)
        assert(result.player.status.head.turns > initialState.player.status.head.turns)
        assert(result.player.status.tail.head.turns > initialState.player.status.tail.head.turns)
      }
    }

    "Given Potion of Regeneration" should {
      val gameState = GameStateGenerator.gameStateWithFastPlayer

      val dungeonBuilder = new SimpleDungeonBuilder()
      val stateCreator = new RandomGameStateCreator(dungeonBuilder)
      val startedGame: GameState = stateCreator.start(gameState)

      val result = selfActionHandler.handle(PotionOfRegeneration)(startedGame)

      "State the action was used" in {
        assert(result.messages.size > startedGame.messages.size)
        assert(result.messages.contains(GameMessage(s"${GameStateGenerator.playerName} drinks a Potion of Regeneration!")))
      }

      "Reduce the player's position" in {
        assert(result.player.position < 100)
      }

      "The Player gains the Regen status" in {
        assert(result.player.status.nonEmpty)
        assert(result.player.status.exists(_.effect == Regen))
      }
    }


    "Given QuickStep" should {
      val gameState = GameStateGenerator.gameStateWithFastPlayer

      val dungeonBuilder = new SimpleDungeonBuilder()
      val stateCreator = new RandomGameStateCreator(dungeonBuilder)
      val startedGame: GameState = stateCreator.start(gameState)

      val result = selfActionHandler.handle(QuickStep)(startedGame)

      "State the action was used" in {
        assert(result.messages.size > startedGame.messages.size)
      }

      "Increase the player's position" in {
        assert(result.player.position > 100)
      }
    }

    "Given an Essence of Vitality" should {
      val gameState = GameStateGenerator.gameStateWithFastPlayer

      val dungeonBuilder = new SimpleDungeonBuilder()
      val stateCreator = new RandomGameStateCreator(dungeonBuilder)
      val startedGame: GameState = stateCreator.start(gameState)

      val result = selfActionHandler.handle(EssenceOfVitality)(startedGame)

      "State the action was used" in {
        assert(result.messages.size > startedGame.messages.size)
      }

      "Not affect the player's position" in {
        assert(result.player.position >= 100)
        assert(startedGame.player.position == result.player.position)
      }

      "The Player's vitality increases" in {
        assert(result.player.stats.vitality > startedGame.player.stats.vitality)
      }

      "Set the essence flag" in {
        assert(result.cards.playedEssence)
      }
    }

    "Given an Essence of Strength" should {
      val gameState = GameStateGenerator.gameStateWithFastPlayer

      val dungeonBuilder = new SimpleDungeonBuilder()
      val stateCreator = new RandomGameStateCreator(dungeonBuilder)
      val startedGame: GameState = stateCreator.start(gameState)

      val result = selfActionHandler.handle(EssenceOfStrength)(startedGame)

      "State the action was used" in {
        assert(result.messages.size > startedGame.messages.size)
      }

      "Not affect the player's position" in {
        assert(result.player.position >= 100)
        assert(startedGame.player.position == result.player.position)
      }

      "The Player's strength increases" in {
        assert(result.player.stats.strength > startedGame.player.stats.strength)
      }

      "Set the essence flag" in {
        assert(result.cards.playedEssence)
      }
    }

    "Given an Essence of Dexterity" should {
      val gameState = GameStateGenerator.gameStateWithFastPlayer

      val dungeonBuilder = new SimpleDungeonBuilder()
      val stateCreator = new RandomGameStateCreator(dungeonBuilder)
      val startedGame: GameState = stateCreator.start(gameState)

      val result = selfActionHandler.handle(EssenceOfDexterity)(startedGame)

      "State the action was used" in {
        assert(result.messages.size > startedGame.messages.size)
      }

      "Not affect the player's position" in {
        assert(result.player.position >= 100)
        assert(startedGame.player.position == result.player.position)
      }

      "The Player's dexterity increases" in {
        assert(result.player.stats.dexterity > startedGame.player.stats.dexterity)
      }

      "Set the essence flag" in {
        assert(result.cards.playedEssence)
      }
    }

    "Given an Essence of Intelligence" should {
      val gameState = GameStateGenerator.gameStateWithFastPlayer

      val dungeonBuilder = new SimpleDungeonBuilder()
      val stateCreator = new RandomGameStateCreator(dungeonBuilder)
      val startedGame: GameState = stateCreator.start(gameState)

      val result = selfActionHandler.handle(EssenceOfIntelligence)(startedGame)

      "State the action was used" in {
        assert(result.messages.size > startedGame.messages.size)
      }

      "Not affect the player's position" in {
        assert(result.player.position >= 100)
        assert(startedGame.player.position == result.player.position)
      }

      "The Player's intellect increases" in {
        assert(result.player.stats.intellect > startedGame.player.stats.intellect)
      }

      "Set the essence flag" in {
        assert(result.cards.playedEssence)
      }
    }
  }
}
