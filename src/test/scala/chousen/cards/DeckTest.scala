package chousen.cards

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
  }
}
