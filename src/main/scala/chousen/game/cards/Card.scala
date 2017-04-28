package chousen.game.cards

trait Card {
  val name: String
}

trait SpellCard extends Card

trait PotionCard extends Card

class FireBallCard extends SpellCard {
  val name = "Fireball Card"
}

class HealWoundsCard extends PotionCard {
  val name = "Heal Wounds Card"
}

