package chousen.game.actions

import java.util.UUID

import chousen.api.data._
import chousen.game.core.RandomGameStateCreator
import chousen.game.dungeon.SimpleDungeonBuilder
import org.scalatest.WordSpec
import chousen.Optics._
import chousen.game.cards.{CardCatalogue, Strength}
import monocle.macros.GenLens

class CardActionHandlerSpec extends WordSpec {

  "Card Action Handler" when {

    val gameState = GameStateGenerator.gameStateWithFastPlayer

    val dungeonBuilder = new SimpleDungeonBuilder()
    val stateCreator = new RandomGameStateCreator(dungeonBuilder)

    "Given a card action" should {
      val startedGame: GameState = stateCreator.start(gameState)

      val result = CardActionHandler.handle(Rummage, None)(startedGame)

      "State the action was used" in {
        assert(result.messages.size > startedGame.messages.size)
      }

    }

    "Given Rummage" should {
      val startedGame: GameState = stateCreator.start(gameState)

      val result = CardActionHandler.handle(Rummage, None)(startedGame)

      "Draw two cards" in {
        assert(result.cards.hand.size > (1 + startedGame.cards.hand.size))
      }
    }

    "Given Acquire" should {
      val startedGame: GameState = stateCreator.start(gameState)

      val result = CardActionHandler.handle(Acquire, None)(startedGame)

      "Draw four cards" in {
        assert(result.cards.hand.size > (3 + startedGame.cards.hand.size))
      }
    }

    "Given Miracle" should {

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
      val game: GameState = stateCreator.start(gameState)
      val cardToDiscard = CardCatalogue.drain

      val startedGame: GameState = HandLens.set(Seq(cardToDiscard))(game)

      val result = CardActionHandler.handle(Trade, Some(cardToDiscard.id))(startedGame)

      "States the action was used" in {
        assert(result.messages.size > startedGame.messages.size)
      }

      "Place new cards into the Player's hand" in {
        assert(result.cards.hand.size > startedGame.cards.hand.size)
      }
    }

    "Given Finders Keepers" should {
      val game: GameState = stateCreator.start(gameState)
      val cardToFind = CardCatalogue.drain

      val startedGame: GameState = DeckLens.modify(_ :+ cardToFind)(game)

      val result = CardActionHandler.handle(FindersKeepers, Some(cardToFind.id))(startedGame)

      "States the action was used" in {
        assert(result.messages.size > startedGame.messages.size)
      }

      "Place new cards into the Player's hand" in {
        assert(result.cards.hand.size > startedGame.cards.hand.size)
      }

      "Place the chosen cards into the Player's hand" in {
        assert(result.cards.hand.exists(_.id == cardToFind.id))
      }

      "Removes the chosen card from the deck pile" in {
        assert(!result.cards.deck.exists(_.id == cardToFind.id))
      }
    }

    "Given Another Time" should {
      val game: GameState = stateCreator.start(gameState)
      val cardToFind = CardCatalogue.drain

      val startedGame: GameState = DiscardLens.modify(_ :+ cardToFind)(game)

      val result = CardActionHandler.handle(AnotherTime, Some(cardToFind.id))(startedGame)

      "States the action was used" in {
        assert(result.messages.size > startedGame.messages.size)
      }

      "Place new cards into the Player's hand" in {
        assert(result.cards.hand.size > startedGame.cards.hand.size)
      }

      "Place the chosen cards into the Player's hand" in {
        assert(result.cards.hand.exists(_.id == cardToFind.id))
      }

      "Removes the chosen card from the discard pile" in {
        assert(!result.cards.discard.exists(_.id == cardToFind.id))
      }
    }

    "Given Manifest Rage" should {
      val game: GameState = stateCreator.start(gameState)
      val cardToDiscard = CardCatalogue.drain

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

    "Given Transmute" should {
      val game: GameState = stateCreator.start(gameState)
      val cardToDiscard = CardCatalogue.rummage
      val abilityToDiscard = CardCatalogue.windStrike
      val equipActionToDiscard = CardCatalogue.club

      val startedGame: GameState = HandLens.set(Seq(cardToDiscard))(game)

      val resultDiscardingCard = CardActionHandler.handle(Transmute, Some(cardToDiscard.id))(startedGame)
      val resultDiscardingAbility = CardActionHandler.handle(Transmute, Some(abilityToDiscard.id))(HandLens.set(Seq(abilityToDiscard))(game))
      val resultDiscardingEquipment = CardActionHandler.handle(Transmute, Some(equipActionToDiscard.id))(HandLens.set(Seq(equipActionToDiscard))(game))

      "States the action was used" in {
        assert(resultDiscardingCard.messages.size > startedGame.messages.size)
      }

      "Give the Player gold" in {
        assert(resultDiscardingCard.player.gold > gameState.player.gold)
      }

      "Give the Player more gold for equipment" in {
        assert(resultDiscardingEquipment.player.gold > resultDiscardingCard.player.gold)
        assert(resultDiscardingEquipment.player.gold > resultDiscardingAbility.player.gold)
      }
    }

    "Given Essence Boost" should {
      val game: GameState = stateCreator.start(gameState)
      val cardToDiscard = CardCatalogue.drain

      val startedGame: GameState = HandLens.set(Seq(cardToDiscard))(game)

      val result = CardActionHandler.handle(EssenceBoost, Some(cardToDiscard.id))(startedGame)

      "State the action was used" in {
        assert(result.messages.size > startedGame.messages.size)
      }

      "Replace the discarded card" in {
        assert(result.cards.hand.size >= startedGame.cards.hand.size)
      }

      "Place essences in the player's hand" in {
        assert(result.cards.hand.count(_.name.contains("Essence")) > startedGame.cards.hand.count(_.name.contains("Essence")))
      }

      "Discard the selected card" in {
        assert(result.cards.discard.size > startedGame.cards.discard.size)
      }
    }


    "Given Armoury" should {
      val game: GameState = stateCreator.start(gameState)
      val cardToDiscard = CardCatalogue.drain
      val builder = new chousen.game.cards.Equipment{}

      val startedGame: GameState = HandLens.set(Seq(cardToDiscard))
        .andThen(DeckLens.modify(_ :+ builder.club))
        .andThen(DeckLens.modify(_ :+ builder.chainmail))(game)

      val result = CardActionHandler.handle(Armoury, None)(startedGame)

      "State the action was used" in {
        assert(result.messages.size > startedGame.messages.size)
      }

      "Not affect the Player's hand" in {
        assert(result.cards.hand.size == startedGame.cards.hand.size)
      }

      "Move equipment cards to the top of the deck" in {
        assert(result.cards.deck.head.action.isInstanceOf[EquipAction])
        assert(result.cards.deck.tail.head.action.isInstanceOf[EquipAction])
      }
    }

    "Given Refresh" should {
      val game: GameState = stateCreator.start(gameState)
      val builder = new Strength {}
      val actionCard = builder.groundStrike

      val startedGame: GameState = HandLens.modify(_ :+ actionCard)(game)

      val result = CardActionHandler.handle(Refresh, None)(startedGame)

      "State the action was used" in {
        assert(result.messages.size > startedGame.messages.size)
      }

      "Affect the Player's hand" in {
        assert(result.cards.hand != startedGame.cards.hand)
      }

      "Discard some cards" in {
        assert(result.cards.discard.size > startedGame.cards.discard.size)
      }

      "Retain any ability cards" in {
        assert(result.cards.hand.exists(_.id == actionCard.id))
      }

    }

    "Given Reduce Requirements" should {
      val game: GameState = stateCreator.start(gameState)
      val cardToAffect =  new chousen.game.cards.Equipment{}.heavyArmour

      val startedGame: GameState = HandLens.set(Seq(cardToAffect))(game)

      val result = CardActionHandler.handle(ReduceRequirements, Some(cardToAffect.id))(startedGame)

      "States the action was used" in {
        assert(result.messages.size > startedGame.messages.size)
      }

      "The affected card remains in the player's hand" in {
        assert(result.cards.hand.exists(_.id == cardToAffect.id))
      }

      "Affect the requirements of the selected card" in {
        assert(result.cards.hand.find(_.id == cardToAffect.id).exists(c => c.requirements != cardToAffect.requirements))
      }


      "Lower the strength requirements of the selected card" in {
        val newCard = result.cards.hand.find(_.id == cardToAffect.id).getOrElse(cardToAffect)

        assert(newCard.requirements.str.getOrElse(99) < cardToAffect.requirements.str.getOrElse(99))
        // Item has no dex requirement
        assert(newCard.requirements.dex.getOrElse(99) == cardToAffect.requirements.dex.getOrElse(99))
      }
    }

    "Given Recharge" should {
      val game: GameState = stateCreator.start(gameState)
      val cardToAffect =  new Strength{}.destruction.copy(charges = Option(1))

      val startedGame: GameState = HandLens.set(Seq(cardToAffect))(game)

      val result = CardActionHandler.handle(Recharge, None)(startedGame)

      "States the action was used" in {
        assert(result.messages.size > startedGame.messages.size)
      }

      "The affected card remains in the player's hand" in {
        assert(result.cards.hand.exists(_.id == cardToAffect.id))
      }

      "Reset the number of charges" in {
        val theCard = result.cards.hand.find(_.id == cardToAffect.id).getOrElse(cardToAffect)
        assert(theCard.charges != cardToAffect.charges)
        assert(theCard.maxCharges == cardToAffect.maxCharges)
        assert(theCard.charges == theCard.maxCharges)
      }
    }

    "Given Charge Up" should {
      val game: GameState = stateCreator.start(gameState)
      val cardToAffect =  new Strength{}.destruction.copy(charges = Option(1))

      val startedGame: GameState = HandLens.set(Seq(cardToAffect))(game)

      val result = CardActionHandler.handle(IncreaseCharges, Some(cardToAffect.id))(startedGame)

      "States the action was used" in {
        assert(result.messages.size > startedGame.messages.size)
      }

      "The affected card remains in the player's hand" in {
        assert(result.cards.hand.exists(_.id == cardToAffect.id))
      }

      "Increases the maximum number of charges" in {
        val theCard = result.cards.hand.find(_.id == cardToAffect.id).getOrElse(cardToAffect)
        assert(theCard.charges.getOrElse(0) > cardToAffect.charges.getOrElse(0))
        assert(theCard.maxCharges.getOrElse(0) > cardToAffect.maxCharges.getOrElse(0))
      }
    }

    "Given BagOfGold" should {
      val startedGame: GameState = stateCreator.start(gameState)

      val result = CardActionHandler.handle(BagOfGold, None)(startedGame)

      "Give the player 30 gold" in {
        assert(result.player.gold > startedGame.player.gold)
        val diff = result.player.gold - startedGame.player.gold
        assert(diff == 30)
      }
    }

    "Given PotOfGold" should {
      val startedGame: GameState = stateCreator.start(gameState)

      val result = CardActionHandler.handle(PotOfGold, None)(startedGame)

      "Give the player 100 gold" in {
        assert(result.player.gold > startedGame.player.gold)
        val diff = result.player.gold - startedGame.player.gold
        assert(diff == 100)
      }
    }

    "Given Brew Poison" should {
      val startedGame: GameState = stateCreator.start(gameState)

      val result = CardActionHandler.handle(BrewPoison, None)(startedGame)

      "Draw two cards" in {
        assert(result.cards.hand.size > (1 + startedGame.cards.hand.size))
      }
    }

    "Given Make Miasma" should {
      import chousen.util.CardsSyntax._
      val startedGame: GameState = stateCreator.start(gameState)

      val initialState = startedGame.copy(cards = startedGame.cards
        .addToHand(CardCatalogue.poison).addToHand(CardCatalogue.flames))

      val result = CardActionHandler.handle(MakeMiasma, None)(initialState)

      "Retain the hand size" in {
        assert(result.cards.hand.size == initialState.cards.hand.size)
      }

      "Remove any potions of poison or flames" in {
        assert(result.cards.hand.forall(_.action != PotionOfFlames))
        assert(result.cards.hand.forall(_.action != PotionOfPoison))
      }

      "Add potions of Miasma to the player's hand" in {
        assert(result.cards.hand.exists(_.action == PotionOfMiasma))
      }
    }

    "Given Make Alkahest" should {
      import chousen.util.CardsSyntax._
      val startedGame: GameState = stateCreator.start(gameState)

      val initialState = startedGame.copy(cards = startedGame.cards
        .addToHand(CardCatalogue.poison).addToHand(CardCatalogue.flames))

      val result = CardActionHandler.handle(MakeAlkahest, None)(initialState)

      "Retain the hand size" in {
        assert(result.cards.hand.size == initialState.cards.hand.size)
      }

      "Remove any potions of poison" in {
        assert(result.cards.hand.forall(_.action != PotionOfPoison))
      }

      "Add potions of Alkahest to the player's hand" in {
        assert(result.cards.hand.exists(_.action == PotionOfAlkahest))
      }
    }


    "Given Purchase Treasure" should {
      val startedGame: GameState = stateCreator.start(gameState)

      val result = CardActionHandler.handle(PurchaseTreasure, None)(startedGame)

      "Draw a single card" in {
      assert(result.cards.hand.size > startedGame.cards.hand.size)
      }

      "Draw a treasure card" in {
        assert(result.cards.hand.exists(_.treasure))
      }
      }

  }
}
