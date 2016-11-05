package chousen.cards

import chousen._

import scala.util.Random


case class Deck(cards: List[Card], discarded: List[Card] = List.empty) {

  def deal: (Hand, Deck) = {
    val (handCards, remainingCards) = this.cards.splitAt(Hand.MAX_SIZE)
    (Hand(handCards), this.copy(cards = remainingCards))
  }

  def draw: Either[Deck, (Card, Deck)] = {
    if (cards.isEmpty) Left(this)
    else {
      val h = cards.head
      val ncs = cards.filterNot(c => c == h)
      Right(h -> copy(cards = ncs))
    }
  }

  def shuffle: Deck = copy(cards = Random.shuffle(cards), discarded = discarded)

  def discard(card: Card) = copy(discarded = card :: discarded)

  def placeAtBottom(card: Card) = copy(cards = cards :+ card)

  def removeTopDiscard: Either[Deck, (Card, Deck)] = {
    if (discarded.isEmpty) Left(this)
    else Right(discarded.head ->
      copy(discarded = discarded.tail))
  }

  def moveTopDiscardToTopOfDeck: Either[Deck, Deck] =
    removeTopDiscard.map(cd => cd._2.copy(cards = cd._1 :: cd._2.cards))
}

object Deck {
  def create = {
    val fourCardRange = Range.inclusive(1, 4).toList

    val magic: List[Card] = fourCardRange.map(i => new HealWoundsCard)
    val heal: List[HealWoundsCard] = fourCardRange.map(i => new HealWoundsCard)

    Deck(magic ++ heal)
  }
}

case class Hand(cards: List[Card]) extends Options[Card] {
  lazy val size = cards.size
  override val items = cards

  def discard(card: Card) = this.copy(cards.filterNot(c => c == card))

  def +(card: Card) = this.copy(cards = card :: cards)

  def choices: Map[String, Card] = {
    statement(optionString)
    options
  }
}

object Hand {
  val MAX_SIZE = 7

  val emptyHand = Hand(List.empty)
}
