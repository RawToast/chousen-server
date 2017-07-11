package chousen.game.cards
import chousen.api.data
import chousen.api.data._
import chousen.game.core.GameStateOptics

import scala.annotation.tailrec
import scala.util.Random

object CardManager extends CardManager {
  val initialCards: Seq[Card] = CardCatalogue.defaultDeck
}


trait CardManager {

  lazy val MAX_HAND_SIZE = 7

  def playCard(card: data.Card)(f: data.Card => GameState): (GameState) => GameState = { game: GameState =>
    import chousen.Implicits._
    game.cards.hand
      .find(_ ~= card)
      .fold(game){c =>
        val ng = f(c)
        if (ng == game) ng
        else {
          // Move to discard
          GameStateOptics.HandLens.modify((cs:Seq[data.Card]) => cs.filterNot(_ ~= c))
            .andThen(GameStateOptics.DiscardLens.modify((ds: Seq[data.Card]) => c +: ds))
            .apply(ng)
        }
      }
  }

  def startGame(cards:Seq[data.Card]): Cards = {
    val shuffledCards = Random.shuffle(Random.shuffle(cards))

    val (hand, deck) = shuffledCards.splitAt(MAX_HAND_SIZE)

    Cards(hand, deck, Seq.empty)
  }

  @tailrec
  final def drawCard(cards: Cards): Cards = {
    if (cards.hand.size < MAX_HAND_SIZE) {
      if (cards.deck.isEmpty) drawCard(cards.copy(deck = cards.discard, discard = Seq.empty))
        else Cards(cards.hand :+ cards.deck.head, cards.deck.tail, cards.discard)
    } else cards
  }

  def moveLastDiscardToTopDeck(cards: Cards): Cards = {
    cards.discard.headOption.fold(cards){ (c: data.Card) =>
      Cards(cards.hand, Seq(c) ++ cards.deck, cards.discard.tail)
    }
  }

  def moveCardToBottomOfDeck(card: data.Card): Cards => Cards = {
    (cards:Cards) =>
      import chousen.Implicits._
      cards.hand
        .find(_ ~= card)
        .fold(cards){ c =>
          Cards(cards.hand.filterNot(_ ~= c),
            cards.deck :+ c,
            cards.discard)
        }
  }


  def discard(card: data.Card): (Cards) => Cards = { cards: Cards =>
    import chousen.Implicits._
    cards.hand.find(_ ~= card).fold(cards) { c =>
      val newHand = cards.hand.filterNot(_ ~= c)
      Cards(newHand, cards.deck, cards.discard :+ c)
    }
  }
}
