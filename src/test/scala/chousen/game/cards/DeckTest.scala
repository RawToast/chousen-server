package chousen.game.cards

import org.scalatest.{Matchers, WordSpec}

class DeckTest extends WordSpec with Matchers {

  "A Deck of cards" when {
    val deck = Deck.create
    "shuffled" should {
      val shuffledDeck:Deck = deck.shuffle
      "have a different order" in {
        deck.cards shouldNot equal(shuffledDeck.cards)
      }
      "not affect the discard pile" in {
        deck.discarded.size should equal(shuffledDeck.discarded.size)
        deck.discarded should equal(shuffledDeck.discarded)
      }
    }
    "dealt to a player" should {
      val (hand, newDeck) = deck.deal

      "give the player 7 cards" in {
        hand.cards.size should equal(Hand.MAX_SIZE)
      }

      "be reduced in size" in {
        newDeck.cards.size shouldNot equal(deck.cards.size)
        newDeck.cards shouldNot equal(deck.cards)
      }
    }
  }
}
