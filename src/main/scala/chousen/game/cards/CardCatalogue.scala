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

  def defaultDeck: Seq[Card] = // 15
    usefulCards ++
      crushingBlow.times(4) ++ groundStrike.times(4) ++ stunningStrike.times(4) ++
      assassinate.times(4) ++ windStrike.times(4) ++ quickStep.times(4) ++
      staticField.times(4) ++ drain.times(4) ++ fireball.times(4) ++
      elixirOfStrength.times(2) ++ elixirOfDexterity.times(2) ++ elixirOfVitality.times(2) ++
      restore.times(4) ++ replace.times(4) ++ miracle.times(4) ++ rummage.times(4) ++ shatter.times(2)

  def magicDeck: Seq[Card] = // 13
    usefulCards ++
    fireball.times(4) ++ magicMissile.times(4) ++ drain.times(4) ++ staticField.times(2) ++ pain.times(2) ++ massDrain.times(4) ++
    assassinate.times(2) ++ quickStep.times(4) ++ shatter.times(2) ++
    elixirOfIntelligence.times(4) ++ elixirOfVitality.times(4) ++
    restore.times(4) ++ replace.times(4) ++ miracle.times(4) ++ rummage.times(4)

  def strengthDeck =
      usefulCards ++ /*4 of might, 4 of stoneSkin, 4 of haste, */
      (4 of crushingBlow) ++ (4 of hamstring) ++ (4 of stunningStrike) ++ (4 of groundStrike) ++ (4 of counter) ++
      (4 of quickStep) ++ (4 of destruction) ++
      (4 of elixirOfStrength) ++ (2 of elixirOfDexterity) ++ (4 of elixirOfVitality) ++
      (4 of rummage) ++ (4 of replace) ++ (2 of restore) ++ (2 of miracle)

  def dexterityDeck = // 15
      usefulCards ++ /*4 of dexterity, 4 of stoneSkin, 4 of haste,*/
      (4 of quickStep) ++ (4 of tripleStrike) ++ (4 of assassinate) ++ (4 of windStrike) ++
      (4 of pain) ++
      (4 of elixirOfDexterity) ++ (4 of elixirOfVitality) ++
      restore.times(4) ++ replace.times(4) ++ miracle.times(2) ++ rummage.times(2)

  def cheeseDeck = // 15
    usefulCards ++
      (4 of assassinate) ++ (4 of windStrike) ++ (4 of quickStep) ++
      (4 of pain) ++ (4 of staticField) ++ (4 of fireball) ++
      (4 of elixirOfDexterity) ++ (4 of elixirOfIntelligence) ++
      restore.times(4) ++ replace.times(4) ++ miracle.times(4) ++ rummage.times(4)

  def strongManDeck = // 10
    usefulCards ++
    massDrain.times(4) ++
    quickStep.times(4) ++ destruction.times(4) ++
    groundStrike.times(4) ++ crushingBlow.times(4) ++ quickAttack.times(4) ++ elixirOfStrength.times(4) ++
      elixirOfDexterity.times(4) ++ elixirOfVitality.times(4) ++
    restore.times(4) ++ replace.times(4) ++ miracle.times(4) ++ rummage.times(4)

  def usefulCards = healWounds.times(4) ++ rarePepe.times(4)
}

sealed trait CardBuilder

trait Potions extends CardBuilder {
  def healWounds: Card = Card(UUID.randomUUID(), "Heal Wounds", "Heals around 30HP", HealWounds)
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
  def massDrain = Card(UUID.randomUUID(), "Mass Drain", "Drains health from multiple enemies and heals the player", MassDrain)
}

trait Strength extends CardBuilder{
  def crushingBlow = Card(UUID.randomUUID(), "Crushing Blow", "Deals heavy damage, but has an increased movement penalty", CrushingBlow)
  def hamstring = Card(UUID.randomUUID(), "Hamstring", "Attack that reduces the speed of a single enemy", Hamstring)
  def stunningStrike = Card(UUID.randomUUID(), "Stunning Strike", "Attack that stuns a single enemy", StunningStrike)
  def groundStrike = Card(UUID.randomUUID(), "Ground Strike", "Slam the ground with your weapon, hitting all enemies and reducing their position", GroundStrike)

  def counter = Card(UUID.randomUUID(), "Counter", "Attack that deals more damage the stronger the enemy", Counter)
  def destruction = Card(UUID.randomUUID(), "Destruction", "Destructive attack that lowers an enemies defenses", Destruction)
}

trait Dexterity extends CardBuilder{
  def quickAttack = Card(UUID.randomUUID(), "Quick Attack", "Attack with reduced movement penalty", QuickAttack)
  def assassinate = Card(UUID.randomUUID(), "Assassinate", "Attack that deals more damage the more hp the enemy is missing", Assassinate)
  def quickStep = Card(UUID.randomUUID(), "Quick Step", "Player can take 2 turns", QuickStep)
  def windStrike = Card(UUID.randomUUID(), "Wind Strike", "Attacks all enemies", WindStrike)
  def tripleStrike = Card(UUID.randomUUID(), "Triple Strike", "Attacks an enemy three times", TripleStrike)
}

trait Utility extends CardBuilder {
  // Cost 100
  def miracle = Card(UUID.randomUUID(), "Miracle", "Draw cards until your hand is full", Miracle)

  // Cost 70
  def rummage = Card(UUID.randomUUID(), "Rummage", "Quickly search the area and draw 2 cards", Rummage)

  // Cost 0
  def replace = Card(UUID.randomUUID(), "Replace", "Instantly replaces the player's hand (will draw at least 3 cards)", Replace)
  def restore = Card(UUID.randomUUID(), "Restore", "Instantly places the top discarded card into your hand", Restore)
}
