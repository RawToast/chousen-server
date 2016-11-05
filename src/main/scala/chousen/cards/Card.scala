package chousen.cards

import chousen.Cast
import chousen.character.{BaseCharacter, CardAction, FireBall, HealWounds}

trait Card {
  val active: CardAction

  def use(user: BaseCharacter, actors:Cast): (Card, Cast) = (this, actors)

//  protected def selectTargets = {
//    ???
//  }
}

trait SpellCard extends Card {
}

trait PotionCard extends Card

class FireBallCard extends SpellCard {
  val active = new FireBall
}

class HealWoundsCard extends PotionCard {
  val active = new HealWounds
}

