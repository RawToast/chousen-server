package chousen.game.cards

import java.util.UUID

import chousen.api.data._

object CardCatalogue extends Potions with PermanentEffects with Magic with Strength with Dexterity with Utility {

  implicit class multiplier(card: Card) {
    def times(n: Int): Seq[Card] = {
      Seq.fill(n)(card.copy(id = UUID.randomUUID()))
    }
  }

  implicit class altMultiplier(n: Int) {
    def of(c: Card): Seq[Card] = {
      Seq.fill(n)(c.copy(id = UUID.randomUUID()))
    }
  }

  def defaultDeck: Seq[Card] = Seq( // 15
    4 of healWounds, /*4 of might, 4 of dexterity, 4 of intelligence, 4 of stoneSkin,*/
    4 of crushingBlow, 4 of groundStrike, 4 of stunningStrike,
    4 of assassinate, 4 of windStrike, 4 of quickStep,
    4 of staticField, 4 of drain, 4 of fireball,
    4 of rarePepe,
    4 of restore, 4 of replace
  ).flatten

  def magicDeck: Seq[Card] = Seq( // 15
    4 of healWounds, /*4 of intelligence, 4 of haste, 2 of stoneSkin,*/
    4 of quickStep, 4 of windStrike, 2 of assassinate,
    4 of fireball, 4 of staticField, 4 of drain, 4 of shatter, 4 of magicMissile, 4 of pain,
    4 of rarePepe, 4 of elixirOfIntelligence, 4 of elixirOfVitality,
    4 of restore
  ).flatten

  def strengthDeck = Seq( // 14
    4 of healWounds, /*4 of might, 4 of stoneSkin, 4 of haste, */
    4 of crushingBlow, 4 of hamstring, 4 of stunningStrike, 4 of groundStrike, 4 of counter,
    4 of quickStep,
    4 of shatter,
    4 of rarePepe, 4 of elixirOfStrength, 4 of elixirOfVitality
  ).flatten

  def dexterityDeck = Seq( // 15
    4 of healWounds, /*4 of dexterity, 4 of stoneSkin, 4 of haste,*/
    4 of quickStep, 4 of tripleStrike, 4 of assassinate, 4 of windStrike,
    4 of pain,
    4 of rarePepe, 4 of elixirOfDexterity, 4 of elixirOfVitality,
    4 of restore, 4 of replace
  ).flatten

  def cheeseDeck = Seq( // 15
    4 of healWounds, /*4 of dexterity, 4 of stoneSkin, 4 of haste,*/
    4 of assassinate, 4 of windStrike, 4 of quickStep,
    4 of pain, 4 of staticField, 4 of fireball,
    4 of rarePepe, 4 of elixirOfDexterity, 4 of elixirOfIntelligence,
    4 of restore, 4 of replace
  ).flatten
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
  def staticField = Card(UUID.randomUUID(), "Static Field", "Reduces all enemies hp by 33%", StaticField)
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

  def counter = Card(UUID.randomUUID(), "Counter", "Attack that deals more damage the stronger the enemy", Counter)
}

trait Dexterity extends CardBuilder{
  def quickAttack = Card(UUID.randomUUID(), "Quick Attack", "Attack with reduced movement penalty", QuickAttack)
  def assassinate = Card(UUID.randomUUID(), "Assassinate", "Attack that deals more damage the more hp the enemy is missing", Assassinate)
  def quickStep = Card(UUID.randomUUID(), "Quick Step", "Player can take 2 turns", QuickStep)
  def windStrike = Card(UUID.randomUUID(), "Wind Strike", "Attacks all enemies", WindStrike)
  def tripleStrike = Card(UUID.randomUUID(), "Triple Strike", "Attacks an enemy three times", TripleStrike)
}

trait Utility extends CardBuilder {
  def rummage = Card(UUID.randomUUID(), "Rummage", "Search the area and draw 2 cards", Rummage)

  def miracle = Card(UUID.randomUUID(), "Miracle", "Draw cards until your hand is full", Miracle)

  def replace = Card(UUID.randomUUID(), "Replace", "Instantly replaces the player's hand", Replace)

  def restore = Card(UUID.randomUUID(), "Restore", "Instantly places the top discarded card into your hand", Restore)
}
