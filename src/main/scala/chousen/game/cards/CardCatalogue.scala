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
}

sealed trait CardBuilder

trait Potions extends CardBuilder {
  def healWounds: Card = Card(UUID.randomUUID(), "Heal Wounds", "Heals 30HP", HealWounds)
}

trait PermanentEffects extends CardBuilder {
  def elixirOfStrength: Card = Card(UUID.randomUUID(), "Elixir of Strength ", "Permanently increases Strength by 2", ElixirOfStrength)
  def elixirOfDexterity: Card = Card(UUID.randomUUID(), "Elixir of Dexterity ", "Permanently increases Dexterity by 2", ElixirOfDexterity)
  def elixirOfIntelligence: Card = Card(UUID.randomUUID(), "Elixir of Intelligence ", "Permanently increases Intelligence by 2", ElixirOfIntelligence)
  def elixirOfVitality: Card = Card(UUID.randomUUID(), "Elixir of Vitality ", "Permanently increases Vitality 30HP", ElixirOfVitality)
  def rarePepe: Card = Card(UUID.randomUUID(), "Rare Pepe ", "Level up", RarePepe)
}

trait Magic extends CardBuilder{
  def fireball = Card(UUID.randomUUID(), "Fireball", "Deals fire damage to all enemies", Fireball)
  def staticField = Card(UUID.randomUUID(), "Static Field", "Reduces all enemies hp by 25%", StaticField)
  def pain = Card(UUID.randomUUID(), "Static Field", "Reduces the hp of a single target by 50%", Pain)
  def shatter = Card(UUID.randomUUID(), "Shatter", "Reduce player to 1hp and deal the same damage to all enemies", Shatter)
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
}
