package chousen.game.actions

import java.util.UUID

import chousen.api.data._
import chousen.game.core.RandomGameStateCreator
import chousen.game.dungeon.SimpleDungeonBuilder
import org.scalatest.WordSpec
import chousen.Optics._
import monocle.macros.GenLens

class CardActionHandlerSpec extends WordSpec {

  "Card Action Handler" when {

    "Given a card action" should {
      val gameState = GameStateGenerator.gameStateWithFastPlayer

      val dungeonBuilder = new SimpleDungeonBuilder()
      val stateCreator = new RandomGameStateCreator(dungeonBuilder)
      val startedGame: GameState = stateCreator.start(gameState)

      val result = CardActionHandler.handle(Rummage, None)(startedGame)

      "State the action was used" in {
        assert(result.messages.size > startedGame.messages.size)
      }

    }

    "Given Rummage" should {
      val gameState = GameStateGenerator.gameStateWithFastPlayer

      val dungeonBuilder = new SimpleDungeonBuilder()
      val stateCreator = new RandomGameStateCreator(dungeonBuilder)
      val startedGame: GameState = stateCreator.start(gameState)

      val result = CardActionHandler.handle(Rummage, None)(startedGame)

      "Draw two cards" in {
        assert(result.cards.hand.size > (1 + startedGame.cards.hand.size))
      }
    }

    "Given Miracle" should {
      val gameState = GameStateGenerator.gameStateWithFastPlayer

      val dungeonBuilder = new SimpleDungeonBuilder()
      val stateCreator = new RandomGameStateCreator(dungeonBuilder)
      val game: GameState = stateCreator.start(gameState)

      val startedGame: GameState = HandLens.set(Seq.empty[Card])(game)

      val result = CardActionHandler.handle(Miracle, None)(startedGame)


      "Draw cards upto the maximum hand size + 1" in {
        assert(result.cards.hand.size > startedGame.cards.hand.size)
        // Afterwards miracle would be discarded
        assert(result.cards.hand.size == 8)
      }
    }

    "Given Replace" should {
      val gameState = GameStateGenerator.gameStateWithFastPlayer

      val dungeonBuilder = new SimpleDungeonBuilder()
      val stateCreator = new RandomGameStateCreator(dungeonBuilder)
      val game: GameState = stateCreator.start(gameState)

      val startedGame: GameState = HandLens.set(Seq.empty[Card])(game)

      val result = CardActionHandler.handle(Replace, None)(startedGame)

      "Give at least 3 cards" in {
        assert(result.cards.hand.size > startedGame.cards.hand.size)
        // Afterwards miracle would be discarded
        assert(result.cards.hand.size >= 3)
      }

      "Replace the current hand" in {
        val result = CardActionHandler.handle(Replace, None)(game)

        assert(result.cards.hand.size == game.cards.hand.size)
        // Afterwards miracle would be discarded
      }
    }

    "Given Restore" should {
      val gameState = GameStateGenerator.gameStateWithFastPlayer

      val dungeonBuilder = new SimpleDungeonBuilder()
      val stateCreator = new RandomGameStateCreator(dungeonBuilder)
      val game: GameState = stateCreator.start(gameState)

      val startedGame: GameState = DiscardLens.set(Seq(Card(UUID.randomUUID(), "Test", "Test", CrushingBlow)))(game)

      val result = CardActionHandler.handle(Restore, None)(startedGame)

      "Place the top discarded card into the player's hand" in {
        assert(!startedGame.cards.hand.exists(_.name == "Test"))
        assert(startedGame.cards.discard.exists(_.name == "Test"))

        assert(result.cards.hand.exists(_.name == "Test"))
        assert(!result.cards.discard.exists(_.name == "Test"))
      }
    }

    "Given Forge Weapon" should {
      val gameState = GameStateGenerator.gameStateWithFastPlayer

      val dungeonBuilder = new SimpleDungeonBuilder()
      val stateCreator = new RandomGameStateCreator(dungeonBuilder)

      val weapon = Card(UUID.randomUUID(), "test", "test", SwordOfIntellect)
      val basicCard = Card(UUID.randomUUID(), "test", "test", HealWounds)

      val startedGame: GameState =
        GenLens[GameState](_.cards.deck).modify(_ :+ weapon)
          .andThen(chousen.Optics.HandLens.modify(_ :+ basicCard))(stateCreator.start(gameState))

      val cardToDiscard = startedGame.cards.hand.filterNot(_.action.isInstanceOf[EquipWeapon]).head

      val result = CardActionHandler.handle(ForgeWeapon, Some(cardToDiscard.id))(startedGame)

      "States the action was used" in {
        assert(result.messages.size > startedGame.messages.size)
      }

      "Retain the same hand size" in {
        // 1 card is discard, another populated
        assert(result.cards.hand.size == startedGame.cards.hand.size)
      }

      "Place a new Weapon card into the Player's hand" in {
        val previousWeapons = startedGame.cards.hand.filter(_.action.isInstanceOf[EquipWeapon])
        val resultWeapons = result.cards.hand.filter(_.action.isInstanceOf[EquipWeapon])
        assert(resultWeapons.nonEmpty)
        assert(resultWeapons.size > previousWeapons.size)
      }
    }

    "Given Forge Armour" should {
      val gameState = GameStateGenerator.gameStateWithFastPlayer

      val dungeonBuilder = new SimpleDungeonBuilder()
      val stateCreator = new RandomGameStateCreator(dungeonBuilder)

      val armourCard = Card(UUID.randomUUID(), "test", "test", Chainmail)
      val basicCard = Card(UUID.randomUUID(), "test", "test", HealWounds)

      val startedGame: GameState = GenLens[GameState](_.cards.deck).modify(_ :+ armourCard)
          .andThen(chousen.Optics.HandLens.modify(_ :+ basicCard))(stateCreator.start(gameState))

      val cardToDiscard = startedGame.cards.hand.filterNot(_.action.isInstanceOf[EquipArmour]).head

      val result = CardActionHandler.handle(ForgeArmour, Some(cardToDiscard.id))(startedGame)

      "States the action was used" in {
        assert(result.messages.size > startedGame.messages.size)
      }

      "Retain the same hand size" in {
        // 1 card is discard, another populated
        assert(result.cards.hand.size == startedGame.cards.hand.size)
      }

      "Place a new Armour card into the Player's hand" in {
        val previousItems = startedGame.cards.hand.filter(_.action.isInstanceOf[EquipArmour])
        val resultWeapons = result.cards.hand.filter(_.action.isInstanceOf[EquipArmour])
        assert(resultWeapons.nonEmpty)
        assert(resultWeapons.size > previousItems.size)
      }
    }

    "Given Trade" should {
      val gameState = GameStateGenerator.gameStateWithFastPlayer

      val dungeonBuilder = new SimpleDungeonBuilder()
      val stateCreator = new RandomGameStateCreator(dungeonBuilder)
      val game: GameState = stateCreator.start(gameState)
      val cardToDiscard = game.cards.hand.head

      val startedGame: GameState = HandLens.set(Seq(cardToDiscard))(game)

      val result = CardActionHandler.handle(Trade, Some(cardToDiscard.id))(startedGame)

      "States the action was used" in {
        assert(result.messages.size > startedGame.messages.size)
      }

      "Place new cards into the Player's hand" in {
        assert(result.cards.hand.size > startedGame.cards.hand.size)
      }
    }

    "Given Manifest Rage" should {
      val gameState = GameStateGenerator.gameStateWithFastPlayer

      val dungeonBuilder = new SimpleDungeonBuilder()
      val stateCreator = new RandomGameStateCreator(dungeonBuilder)
      val game: GameState = stateCreator.start(gameState)
      val cardToDiscard = game.cards.hand.head

      val startedGame: GameState = HandLens.set(Seq(cardToDiscard))(game)

      val result = CardActionHandler.handle(ManifestRage, Some(cardToDiscard.id))(startedGame)

      "States the action was used" in {
        assert(result.messages.size > startedGame.messages.size)
      }

      "Replace the discarded card" in {
        assert(result.cards.hand.size == startedGame.cards.hand.size)
      }

      "Place a Potion of Rage into the Player's deck" in {
        assert(result.cards.deck.size > startedGame.cards.deck.size)
        assert(result.cards.deck.exists(_.action == PotionOfRage))
      }

      "Discard the selected card" in {
        assert(result.cards.discard.size > startedGame.cards.discard.size)
      }
    }

  }
}
