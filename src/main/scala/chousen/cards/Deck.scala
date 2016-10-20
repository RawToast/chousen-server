package chousen.cards

import chousen.Actors
import chousen.character._

import scala.util.Random

case class Deck(cards: List[Card], discarded: List[Card] = List.empty) {

  def deal: Hand = ???

  def shuffle: Deck = copy(cards=Random.shuffle(cards), discarded=discarded)

}

object Deck {
  def create = {
    val fourCardRange = Range.inclusive(1, 4).toList

    val magic: List[Card] = fourCardRange.map(i => new HealWoundsCard)
    val heal: List[HealWoundsCard] = fourCardRange.map(i => new HealWoundsCard)

    Deck(magic ++ heal)
  }
}

case class Hand(cards: List[Card])

trait Card {
  val active: CardAction

  def use(user: BaseCharacter, target: Set[BaseCharacter], bystanders: Option[Set[BaseCharacter]]): Actors =
    active.complete(user, target, bystanders)
}

trait SpellCard extends Card

trait PotionCard extends Card

class FireBallCard extends SpellCard {
  val active = new FireBall
}

class HealWoundsCard extends PotionCard {
  val active = new HealWounds
}


