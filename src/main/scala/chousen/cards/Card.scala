package chousen.cards

import chousen.Actors
import chousen.character.{BaseCharacter, CardAction, FireBall, HealWounds}

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

