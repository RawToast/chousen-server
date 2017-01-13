package chousen.cards

import chousen.Cast
import chousen.character.{BaseCharacter, CardAction, FireBall, HealWounds}

trait Card {
  val active: CardAction

  override def toString: String = active.name + " Card"

  def use(user: BaseCharacter, actors:Cast): (Card, Cast) = (this, actors)
}

trait SpellCard extends Card

trait PotionCard extends Card

class FireBallCard extends SpellCard {
  val active = new FireBall
}

class HealWoundsCard extends PotionCard {
  val active = new HealWounds
}

