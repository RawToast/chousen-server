package chousen.cards

import org.scalatest.{Matchers, WordSpec}

import scala.annotation.tailrec

class DeckManagerTest extends WordSpec with Matchers {

  "The Deck manager" when {

    val deck = Deck.create
    val initManager = DeckManager.createManagerForNewGame(deck)
    val deckManager: DeckManager = initManager.startGame

    "starting a new game" should {
      "shuffle the cards" in {
        // FIXME Not a real test for shuffling, spy object may be best here
        deckManager.deck shouldNot equal(initManager.deck)
      }

      "deal cards to the player" in {
        deckManager.hand.cards should have size Hand.MAX_SIZE
      }

      "reduce the size of the remaining deck" in {
        deckManager.deck.cards.size should be < initManager.deck.cards.size
      }
    }

    "the player discards a card" should {

      val cardToDiscard = deckManager.hand.cards.head
      val newManager: DeckManager = deckManager.discard(cardToDiscard)

      "remove the card from the players hand" in {
        newManager.hand.size shouldBe <(deckManager.hand.size)
        newManager.hand.cards shouldNot contain(cardToDiscard)
      }

      "discard the card" in {
        deckManager.deck.discarded shouldNot contain(cardToDiscard)
        newManager.deck.discarded should contain(cardToDiscard)
        newManager.deck.discarded.size shouldBe >(deckManager.deck.discarded.size)

        // Should be head card
        newManager.deck.discarded.head should equal(cardToDiscard)
      }
    }

    "the player draws a card" should {

      val testDm: DeckManager = DeckManager.get(deckManager.drawCard)

      "add a card to the players hand" in {
        testDm.hand.size shouldBe >(deckManager.hand.size)
        testDm.hand.cards.diff(deckManager.hand.cards) should have size 1
      }

      "reduce the size of the deck" in {
        testDm.deck.cards.size shouldBe <(deckManager.deck.cards.size)
        testDm.deck.discarded.size should equal(deckManager.deck.discarded.size)
      }

      "give no cards when none remain" in {
        @tailrec
        def drawUntil(dm: DeckManager): DeckManager = {
          if (dm.deck.cards.isEmpty) dm
          else drawUntil(DeckManager.get(dm.drawCard))
        }

        val emptyDeck = drawUntil(deckManager)
        val testDm = emptyDeck.drawCard

        // Really should work!
        testDm.isLeft should be(true)
        testDm.isRight should be(false)

        // Just get what we have been given.
        val testDeck: DeckManager = DeckManager.get(testDm)

        emptyDeck.hand.size should equal(testDeck.hand.size)
        emptyDeck.deck.cards.size should equal(testDeck.deck.cards.size)
        emptyDeck.deck.discarded.size should equal(testDeck.deck.discarded.size)
      }
    }

    "repopulating from the discard pile" should {

      val cardToDiscard = deckManager.hand.cards.head

      val dm = deckManager.discard(cardToDiscard)

      val testXor = dm.populateFromDiscard
      val testDm = DeckManager.get(testXor)

      "remove the top card from the discard pile when there is one" in {
        dm.deck.discarded.size should be > 0
        testXor.isRight shouldBe true

        testDm.deck.discarded.size should be < dm.deck.discarded.size //1
        testDm.deck.discarded shouldNot contain(cardToDiscard)
      }

      "place it within the deck" in {
        testDm.deck.cards.size should be > dm.deck.cards.size
        testDm.deck.cards.head should equal(cardToDiscard)
        testDm.deck.cards should contain(cardToDiscard)
      }

      "return failure when no cards have been discarded" in {
        val existingSize = deckManager.deck.cards.size
        deckManager.deck.discarded should have size 0
        val xorDm = deckManager.populateFromDiscard

        xorDm.isLeft shouldBe true
        xorDm.isRight shouldBe false

        DeckManager.get(xorDm).deck.discarded should have size 0
        DeckManager.get(xorDm).deck.cards should have size existingSize
      }
    }

    "repopulating from the players hand" should {

      val cardToUse = deckManager.hand.cards.head
      val testDeck = deckManager.moveCardFromHandToBottomOfDeck(cardToUse)

      "take the chosen card from the players hand" in {
        testDeck.hand.size should be < deckManager.hand.size
        testDeck.deck.cards.size should be > deckManager.deck.cards.size

        testDeck.hand.cards shouldNot contain(cardToUse)
      }

      "place it at the bottom of the deck" in {
        testDeck.deck.cards.last should equal(cardToUse)
      }
    }
  }
}
