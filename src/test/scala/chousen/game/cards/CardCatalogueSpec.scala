package chousen.game.cards

import chousen.api.data.Card
import org.scalatest.WordSpec

class CardCatalogueSpec extends WordSpec {

  "Card Catalogue" should {

    val catalogue = CardCatalogue


    "Provide methods to create cards" in {
      val card1 = catalogue.replace
      val card2 = catalogue.forgeArmour
      val card3 = catalogue.forgeWeapon
      val card4 = catalogue.restore

      assert(card1.name == "Replace")
      assert(Set(card1.action, card2.action, card3.action, card4.action).size == 4)
    }

    "Contain pre-made decks" that {
      val deck1 = catalogue.fighterDeck
      val deck2 = catalogue.berserkerDeck
      val deck3 = catalogue.mageDeck
      val deck4 = catalogue.rogueDeck
      val deck5 = catalogue.tricksterDeck

      standardAssertions("Fighter", deck1)
      standardAssertions("Berserker", deck2)
      standardAssertions("Mage", deck3)
      standardAssertions("Rogue", deck4)
      standardAssertions("Trickster", deck5)
    }


    def standardAssertions(name: String, deck: Seq[Card]) = {
      s"$name deck Has a size of 60 cards" in {
        val size = deck.size
        assert(size == 60)
      }
    }

  }

}
