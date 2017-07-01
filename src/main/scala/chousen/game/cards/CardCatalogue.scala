package chousen.game.cards

import java.util.UUID

import chousen.api.data._

object CardCatalogue extends Potions with PermanentEffects with Magic with Strength with Dexterity {
  def defaultDeck = Seq(healWounds, assassinate, windStrike, staticField, drain, fireball, quickStep, rarePepe) ++
    Seq(healWounds, stunningStrike, assassinate, windStrike, staticField, drain, fireball, quickStep, rarePepe) ++
    Seq(healWounds, stunningStrike, assassinate, windStrike, staticField, drain, fireball, quickStep, rarePepe) ++
    Seq(healWounds, stunningStrike, assassinate, windStrike, staticField, drain, fireball, quickStep, rarePepe) ++
      Seq(might, dexterity, intelligence, stoneSkin)

  def magicDeck = Seq(fireball, healWounds, pain, shatter, magicMissile, drain, quickStep, elixirOfIntelligence, rarePepe) ++
    Seq(fireball, healWounds, pain, shatter, magicMissile, drain, quickStep, elixirOfIntelligence, rarePepe) ++
    Seq(fireball, healWounds, pain, shatter, magicMissile, drain, quickStep, elixirOfIntelligence, rarePepe)  ++
    Seq(fireball, healWounds, pain, shatter, magicMissile, drain, quickStep, elixirOfIntelligence, rarePepe) ++
    Seq(staticField, staticField, assassinate, assassinate)

  def strengthDeck = Seq(crushingBlow, hamstring, stunningStrike, groundStrike, healWounds, elixirOfStrength, rarePepe) ++
    Seq(crushingBlow, hamstring, stunningStrike, groundStrike, healWounds, elixirOfStrength, rarePepe) ++
    Seq(crushingBlow, hamstring, stunningStrike, groundStrike, healWounds, elixirOfStrength, rarePepe)  ++
    Seq(crushingBlow, hamstring, stunningStrike, groundStrike, healWounds, elixirOfStrength, rarePepe) ++
    Seq(might, might, assassinate, assassinate, shatter, shatter, pain, pain)

  def dexterityDeck = Seq(quickAttack, tripleStrike, assassinate, windStrike, healWounds, quickStep, elixirOfDexterity, rarePepe) ++
    Seq(quickAttack, crushingBlow, assassinate, windStrike, healWounds, quickStep, elixirOfDexterity, rarePepe) ++
    Seq(quickAttack, crushingBlow, assassinate, windStrike, healWounds, quickStep, elixirOfDexterity, rarePepe)  ++
    Seq(quickAttack, crushingBlow, assassinate, windStrike, healWounds, quickStep, elixirOfDexterity, rarePepe) ++
    Seq(dexterity, dexterity, pain, pain, pain)

  def shacoDeck = Seq(rarePepe, healWounds, quickStep, assassinate, pain, windStrike, rarePepe) ++
    Seq(tripleStrike, tripleStrike, quickAttack, quickAttack) ++
    Seq(shatter, shatter) ++
    Seq(elixirOfDexterity, elixirOfDexterity, elixirOfIntelligence, elixirOfIntelligence)
}

sealed trait CardBuilder

trait Potions extends CardBuilder {
  def healWounds: Card = Card(UUID.randomUUID(), "Heal Wounds", "Heals around 30HP", HealWounds)
  def haste: Card = Card(UUID.randomUUID(), "Potion of Haste", "Temporarily increases player speed", Haste)
  def might: Card = Card(UUID.randomUUID(), "Potion of Might", "Temporarily increases player strength", PotionOfMight)
  def intelligence: Card = Card(UUID.randomUUID(), "Potion of Intelligence", "Temporarily increases player intelligence", PotionOfIntelligence)
  def stoneSkin: Card = Card(UUID.randomUUID(), "Stone Skin", "Temporarily increases player defence", PotionOfStoneSkin)
  def dexterity: Card = Card(UUID.randomUUID(), "Potion of Dexterity", "Temporarily increases player dexterity", PotionOfDexterity)
}

trait PermanentEffects extends CardBuilder {
  def elixirOfStrength: Card = Card(UUID.randomUUID(), "Elixir of Strength ", "Permanently increases Strength by 3", ElixirOfStrength)
  def elixirOfDexterity: Card = Card(UUID.randomUUID(), "Elixir of Dexterity ", "Permanently increases Dexterity by 3", ElixirOfDexterity)
  def elixirOfIntelligence: Card = Card(UUID.randomUUID(), "Elixir of Intelligence ", "Permanently increases Intelligence by 3", ElixirOfIntelligence)
  def elixirOfVitality: Card = Card(UUID.randomUUID(), "Elixir of Vitality ", "Permanently increases Vitality by 3", ElixirOfVitality)
  def rarePepe: Card = Card(UUID.randomUUID(), "Rare Pepe ", "Level up, increasing all stats", RarePepe)
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
