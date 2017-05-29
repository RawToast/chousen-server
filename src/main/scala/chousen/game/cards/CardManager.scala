package chousen.game.cards
import chousen.api.data
import chousen.api.data._
import chousen.game.core.GameStateOptics

import scala.util.Random

object CardManager extends CardManager {

  val initialCards = CardCatalogue.defaultDeck
}


trait CardManager {

  lazy val MAX_HAND_SIZE = 7

  def playCard(card: data.Card)(f: data.Card => GameState): (GameState) => GameState = { game: GameState =>
    import chousen.api.types.Implicits._
    game.cards.hand
      .find(_ ~= card)
      .fold(game){c =>
        val ng = f(c)
        if (ng == game) ng
        else {


          // Move to discard



          GameStateOptics.HandLens.modify((cs:Seq[data.Card]) => cs.filterNot(_ ~= c)).apply(ng)
        }
      }
  }

  def startGame(cards:Seq[data.Card]): Cards = {

    val shuffledCards = Random.shuffle(cards)

    val (hand, deck) = shuffledCards.splitAt(MAX_HAND_SIZE)

    Cards(hand, deck, Seq.empty)
  }

  def drawCard(cards: Cards): Cards = {
    cards.deck match {
      case h :: t => Cards(cards.hand :+ h, t, cards.discard)
      case Nil => cards
    }
  }

  def moveLastDiscardToTopDeck(cards: Cards): Cards = {
    cards.discard.headOption.fold(cards){ (c: data.Card) =>
      Cards(cards.hand, Seq(c) ++ cards.deck, cards.discard.tail)
    }

  }

  def moveCardToBottomOfDeck(card: data.Card): Cards => Cards = {
    (cards:Cards) =>
      import chousen.api.types.Implicits._
      cards.hand
        .find(_ ~= card)
        .fold(cards){ c =>
          Cards(cards.hand.filterNot(_ ~= c),
            cards.deck :+ c,
            cards.discard)
        }
  }


  def discard(card: data.Card) = { cards: Cards =>
    import chousen.api.types.Implicits._
    cards.hand.find(_ ~= card).fold(cards) { c =>
      val newHand = cards.hand.filterNot(_ ~= c)
      Cards(newHand, cards.deck, cards.discard :+ c)
    }
  }
}
