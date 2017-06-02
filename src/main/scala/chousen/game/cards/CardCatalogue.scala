package chousen.game.cards

import java.util.UUID

import chousen.api.data._

object CardCatalogue extends Potions with PermanentEffects with Magic with Strength with Dexterity {
  def defaultDeck = Seq(healWounds, fireball, staticField, pain, shatter, crushingBlow, hamstring, stunningStrike, groundStrike, quickAttack, assassinate, quickStep, windStrike) ++
      Seq(healWounds, fireball, staticField, pain, shatter, crushingBlow, hamstring, stunningStrike, groundStrike, quickAttack, assassinate, quickStep, windStrike) ++
      Seq(healWounds, fireball, staticField, pain, shatter, crushingBlow, hamstring, stunningStrike, groundStrike, quickAttack, assassinate, quickStep, windStrike) ++
      Seq(healWounds, fireball, staticField, pain, shatter, crushingBlow, hamstring, stunningStrike, groundStrike, quickAttack, assassinate, quickStep, windStrike) ++
      Seq(elixirOfStrength, elixirOfDexterity, elixirOfIntelligence, elixirOfVitality, rarePepe) ++
      Seq(elixirOfStrength, elixirOfDexterity, elixirOfIntelligence, elixirOfVitality, rarePepe)

  def magicDeck = Seq(fireball, healWounds, pain, shatter, magicMissile, drain, quickStep, elixirOfIntelligence, rarePepe) ++
    Seq(fireball, healWounds, pain, shatter, magicMissile, drain, quickStep, elixirOfIntelligence, rarePepe) ++
    Seq(fireball, healWounds, pain, shatter, magicMissile, drain, quickStep, elixirOfIntelligence, rarePepe)  ++
    Seq(fireball, healWounds, pain, shatter, magicMissile, drain, quickStep, elixirOfIntelligence, rarePepe) ++
    Seq(staticField, staticField, assassinate, assassinate)

  def strengthDeck = Seq(crushingBlow, hamstring, stunningStrike, groundStrike, healWounds, quickStep, elixirOfStrength, rarePepe) ++
    Seq(crushingBlow, hamstring, stunningStrike, groundStrike, healWounds, quickStep, elixirOfStrength, rarePepe) ++
    Seq(crushingBlow, hamstring, stunningStrike, groundStrike, healWounds, quickStep, elixirOfStrength, rarePepe)  ++
    Seq(crushingBlow, hamstring, stunningStrike, groundStrike, healWounds, quickStep, elixirOfStrength, rarePepe) ++
    Seq(elixirOfDexterity, elixirOfDexterity, assassinate, assassinate, shatter, shatter, pain, pain)

  def dexterityDeck = Seq(quickAttack, tripleStrike, assassinate, windStrike, healWounds, quickStep, elixirOfDexterity, rarePepe) ++
    Seq(quickAttack, crushingBlow, assassinate, windStrike, healWounds, quickStep, elixirOfDexterity, rarePepe) ++
    Seq(quickAttack, crushingBlow, assassinate, windStrike, healWounds, quickStep, elixirOfDexterity, rarePepe)  ++
    Seq(quickAttack, crushingBlow, assassinate, windStrike, healWounds, quickStep, elixirOfDexterity, rarePepe) ++
    Seq(elixirOfStrength, elixirOfStrength, stunningStrike, stunningStrike, shatter, shatter, pain, pain)
}

sealed trait CardBuilder

trait Potions extends CardBuilder {
  def healWounds: Card = Card(UUID.randomUUID(), "Heal Wounds", "Heals 30HP", HealWounds)
}

trait PermanentEffects extends CardBuilder {
  def elixirOfStrength: Card = Card(UUID.randomUUID(), "Elixir of Strength ", "Permanently increases Strength by 2", ElixirOfStrength)
  def elixirOfDexterity: Card = Card(UUID.randomUUID(), "Elixir of Dexterity ", "Permanently increases Dexterity by 2", ElixirOfDexterity)
  def elixirOfIntelligence: Card = Card(UUID.randomUUID(), "Elixir of Intelligence ", "Permanently increases Intelligence by 2", ElixirOfIntelligence)
  def elixirOfVitality: Card = Card(UUID.randomUUID(), "Elixir of Vitality ", "Permanently increases Vitality by 2", ElixirOfVitality)
  def rarePepe: Card = Card(UUID.randomUUID(), "Rare Pepe ", "Level up", RarePepe)
}

trait Magic extends CardBuilder{
  def fireball = Card(UUID.randomUUID(), "Fireball", "Deals fire damage to all enemies", Fireball)
  def staticField = Card(UUID.randomUUID(), "Static Field", "Reduces all enemies hp by 25%", StaticField)
  def pain = Card(UUID.randomUUID(), "Pain", "Reduces the hp of a single target by 50%", Pain)
  def shatter = Card(UUID.randomUUID(), "Shatter", "Reduce player to 1hp and deal the same damage to all enemies", Shatter)
  def magicMissile = Card(UUID.randomUUID(), "Magic Missile", "Deals magic damage to a single enemy", MagicMissile)
  def drain = Card(UUID.randomUUID(), "Drain", "Drains health from an enemy and heals the player", Drain)
}

trait Strength extends CardBuilder{
  def crushingBlow = Card(UUID.randomUUID(), "Crushing Blow", "Deals heavy damage, but has an increased movement penalty", CrushingBlow)
  def hamstring = Card(UUID.randomUUID(), "Hamstring", "Attack that reduces the speed of a single enemy", Hamstring)
  def stunningStrike = Card(UUID.randomUUID(), "Stunning Strike", "Attack that stuns a single enemy", StunningStrike)
  def groundStrike = Card(UUID.randomUUID(), "Ground Strike", "Slam the ground with your weapon, hitting all enemies and reducing their position", GroundStrike)
}

trait Dexterity extends CardBuilder{
  def quickAttack = Card(UUID.randomUUID(), "Quick Attack", "Attack with reduced movement penalty", QuickAttack)
  def assassinate = Card(UUID.randomUUID(), "Assassinate", "Attack that deals more damage the more hp the enemy is missing", Assassinate)
  def quickStep = Card(UUID.randomUUID(), "Quick Step", "Player can take 2 turns", QuickStep)
  def windStrike = Card(UUID.randomUUID(), "Wind Strike", "Attacks all enemies", WindStrike)
  def tripleStrike = Card(UUID.randomUUID(), "Triple Strike", "Attacks an enemy three times", TripleStrike)
}
