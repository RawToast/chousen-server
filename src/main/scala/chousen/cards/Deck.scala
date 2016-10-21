package chousen.cards

import cats.data.Xor

import scala.util.Random

case class Deck(cards: List[Card], discarded: List[Card] = List.empty) {

  def deal: (Hand, Deck) = {
    val (handCards, remainingCards) = this.cards.splitAt(Hand.MAX_SIZE)
    (Hand(handCards), this.copy(cards = remainingCards))
  }

  def draw: Xor[Deck, (Card, Deck)] = {
    if (cards.isEmpty) Xor.Left(this)
    else {
      val h = cards.head
      val ncs = cards.filterNot(c => c == h)
      Xor.Right(h, copy(cards = ncs))
    }
  }

  def shuffle: Deck = copy(cards = Random.shuffle(cards), discarded = discarded)

  def discard(card: Card) = copy(discarded = card :: discarded)

  def placeAtBottom(card: Card) = copy(cards = cards :+ card)

  def removeTopDiscard: Xor[Deck, (Card, Deck)] = {
    if (discarded.isEmpty) Xor.Left(this)
    else Xor.Right(discarded.head,
      copy(discarded = discarded.tail))
  }

  def moveTopDiscardToTopOfDeck: Xor[Deck, Deck] =
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

case class Hand(cards: List[Card]) {
  lazy val size = cards.size

  def discard(card: Card) = this.copy(cards.filterNot(c => c == card))

  def +(card: Card) = this.copy(cards = card :: cards)
}

object Hand {
  val MAX_SIZE = 7

  val emptyHand = Hand(List.empty)
}