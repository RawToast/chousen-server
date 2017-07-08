package chousen.game.cards

import chousen.api.data
import chousen.api.data.Cards
import org.scalatest.{Matchers, WordSpec}


class CardManagerSpec extends WordSpec with Matchers {

  "The Deck manager" when {

    val cards: Seq[data.Card] = CardManager.initialCards
    val shuffledCards: Cards = CardManager.startGame(cards)

    "starting a new makeChar" should {
      "shuffle the cards" in {
        // FIXME Not a real test for shuffling, spy object may be best here
        shuffledCards shouldNot equal(Cards(cards, Seq.empty, Seq.empty))
      }

      "deal cards to the player" in {
        shuffledCards.hand should have size CardManager.MAX_HAND_SIZE.toLong
      }

      "increase the size of the remaining deck" in {
        shuffledCards.deck.size should be > 0
      }
    }

    "the player discards a card" should {

      val cardToDiscard = shuffledCards.hand.head
      val newCards: Cards = CardManager.discard(cardToDiscard)(shuffledCards)

      "remove the card from the players hand" in {
        newCards.hand.size shouldBe <(shuffledCards.hand.size)
        newCards.hand shouldNot contain(cardToDiscard)
      }

      "discard the card" in {
        shuffledCards.discard shouldNot contain(cardToDiscard)
        newCards.discard should contain(cardToDiscard)
        newCards.discard.size shouldBe >(shuffledCards.discard.size)

        // Should be head card
        newCards.discard.head should equal(cardToDiscard)
      }
    }

    "the player draws a card" should {

      val fullCards: Cards = CardManager.drawCard(shuffledCards)

      "not add a card if the hand is full" in {
        fullCards.hand.size shouldBe shuffledCards.hand.size
        fullCards.hand.diff(shuffledCards.hand) should have size 0
      }

      val nonFullHandCards = fullCards.copy(hand = fullCards.hand.tail)
      val newCards = CardManager.drawCard(nonFullHandCards)


      "add a card to the players hand" in {
        newCards.hand.size shouldBe >(nonFullHandCards.hand.size)
        newCards.hand.diff(nonFullHandCards.hand) should have size 1
      }

      "reduce the size of the deck" in {
        newCards.deck.size shouldBe <(nonFullHandCards.deck.size)
        newCards.discard.size should equal(nonFullHandCards.discard.size)
      }

      "refresh the deck from the discard pile when required" in {
        val emptyDeck = nonFullHandCards.copy(deck = Seq.empty, discard = nonFullHandCards.deck)
        val testDeck = CardManager.drawCard(emptyDeck)

        emptyDeck.hand.size shouldBe < (testDeck.hand.size)
        emptyDeck.deck.size shouldBe < (testDeck.deck.size)
        emptyDeck.discard.size shouldBe > (testDeck.discard.size)
      }
    }

    "repopulating from the discard pile" should {

      val cardToDiscard = shuffledCards.hand.head

      val afterDiscard: Cards = CardManager.discard(cardToDiscard)(shuffledCards)

      val afterPopulate = CardManager.moveLastDiscardToTopDeck(afterDiscard)

      "remove the top card from the discard pile when there is one" in {
        afterDiscard.discard.size should be > 0

        afterPopulate.discard.size should be < afterDiscard.discard.size //1
        afterPopulate.discard shouldNot contain(cardToDiscard)
      }

      "place it on top of the deck" in {
        afterPopulate.deck.size should be > afterDiscard.deck.size
        afterPopulate.deck.head should equal(cardToDiscard)
      }

      "do nothing when no cards have been discarded" in {
        val postPopulate = CardManager.moveLastDiscardToTopDeck(shuffledCards)

        postPopulate.deck shouldBe shuffledCards.deck
        postPopulate.discard shouldBe shuffledCards.discard
        postPopulate.hand shouldBe shuffledCards.hand
      }
    }

    "repopulating from the players hand" should {

      val cardToUse = shuffledCards.hand.head

      val testDeck = CardManager.moveCardToBottomOfDeck(cardToUse)(shuffledCards)


      "take the chosen card from the players hand" in {
        testDeck.hand.size should be < shuffledCards.hand.size
        testDeck.deck.size should be > shuffledCards.deck.size

        testDeck.hand shouldNot contain(cardToUse)
      }

      "place it at the bottom of the deck" in {
        testDeck.deck.last should equal(cardToUse)
      }
    }
  }
}
