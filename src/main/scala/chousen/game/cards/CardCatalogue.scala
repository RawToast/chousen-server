package chousen.game.cards

import java.util.UUID

import chousen.api.data._

object CardCatalogue extends Potions with PermanentEffects with Magic with Strength with Dexterity with Utility with CampFire {


  def defaultDeck: Seq[Card] = // 15
      Seq(healWounds, healWounds, healWounds, healWounds,
        rarePepe, rarePepe, rarePepe, rarePepe,
        might, might, might, might,
        stoneSkin, stoneSkin, stoneSkin, stoneSkin,
        haste, haste, haste, haste,
        elixirOfStrength, elixirOfStrength, elixirOfStrength, elixirOfStrength,
        elixirOfVitality, elixirOfVitality, elixirOfVitality, elixirOfVitality,

        crushingBlow, crushingBlow, crushingBlow, crushingBlow,
        hamstring, hamstring, hamstring, hamstring,
        stunningStrike, stunningStrike, stunningStrike, stunningStrike,
        groundStrike, groundStrike, groundStrike, groundStrike,
        counter, counter, counter, counter,
        destruction, destruction, destruction, destruction,

        rummage, rummage, rummage, rummage,
        replace, replace, replace, replace,
        restore, restore,
        miracle, miracle
      )


//  def cheeseDeck: Seq[Card] = // 15
//    usefulCards ++
//      (4 of assassinate) ++ (4 of windStrike) ++ (4 of quickStep) ++
//      (4 of pain) ++ (4 of staticField) ++ (4 of fireball) ++
//      (4 of elixirOfDexterity) ++ (4 of elixirOfIntelligence) ++
//      restore.times(4) ++ replace.times(4) ++ miracle.times(4) ++ rummage.times(4)

//  def strongManDeck: Seq[Card] = // 10
//    usefulCards ++
//    haste.times(4) ++ might.times(4) ++ stoneSkin.times(4) ++ dexterity.times(4) ++
//    quickStep.times(4) ++ destruction.times(4) ++
//    groundStrike.times(4) ++ crushingBlow.times(4) ++ quickAttack.times(4) ++ elixirOfStrength.times(4) ++
//      elixirOfDexterity.times(4) ++ elixirOfVitality.times(4) ++
//    restore.times(4) ++ replace.times(4) ++ miracle.times(4) ++ rummage.times(4)

  def passiveCards: Seq[Card] = Seq(rest, explore, restAndExplore)
}

sealed trait CardBuilder {
  def mkCard(name: String, description: String, action: Action) = Card(UUID.randomUUID(), name, description, action)
}

trait Potions extends CardBuilder {
  def healWounds: Card = mkCard("Heal Wounds", "Heals around 30HP", HealWounds)
  def haste: Card = Card(UUID.randomUUID(), "Potion of Haste", "Temporarily increases player speed", Haste)
  def might: Card = Card(UUID.randomUUID(), "Potion of Might", "Temporarily increases player strength", PotionOfMight)
  def intelligence: Card = Card(UUID.randomUUID(), "Potion of Intelligence", "Temporarily increases player intelligence", PotionOfIntelligence)
  def stoneSkin: Card = Card(UUID.randomUUID(), "Stone Skin", "Temporarily increases player defence", PotionOfStoneSkin)
  def dexterity: Card = Card(UUID.randomUUID(), "Potion of Dexterity", "Temporarily increases player dexterity", PotionOfDexterity)
}

trait PermanentEffects extends CardBuilder {
  def elixirOfStrength: Card = mkCard("Elixir of Strength ", "Permanently increases Strength by 3", ElixirOfStrength)
  def elixirOfDexterity: Card = mkCard("Elixir of Dexterity ", "Permanently increases Dexterity by 3", ElixirOfDexterity)
  def elixirOfIntelligence: Card = mkCard("Elixir of Intelligence ", "Permanently increases Intelligence by 3", ElixirOfIntelligence)
  def elixirOfVitality: Card = mkCard("Elixir of Vitality ", "Permanently increases Vitality by 3", ElixirOfVitality)
  def rarePepe: Card = mkCard("Rare Pepe ", "Level up, increasing all stats", RarePepe)
}

trait Magic extends CardBuilder{
//  def fireball: Card = mkCard("Fireball", "Deals fire damage to all enemies", Fireball)
//  def staticField: Card = mkCard("Static Field", "Reduces all enemies hp by 33%", StaticField)
//  def pain: Card = mkCard("Pain", "Reduces the hp of a single target by 50%", Pain)
//  def shatter: Card = mkCard("Shatter", "Reduce player to 1hp and deal the same damage to all enemies", Shatter)
//  def magicMissile: Card = mkCard("Magic Missile", "Deals magic damage to a single enemy", MagicMissile)
//  def drain: Card = mkCard("Drain", "Drains health from an enemy and heals the player", Drain)
//  def massDrain: Card = mkCard("Mass Drain", "Drains health from multiple enemies and heals the player", MassDrain)
}

trait Strength extends CardBuilder{
  def crushingBlow: Card = mkCard("Crushing Blow", "Deals heavy damage, but has an increased movement penalty", CrushingBlow)
  def hamstring: Card = mkCard("Hamstring", "Attack that reduces the speed of a single enemy", Hamstring)
  def stunningStrike: Card = mkCard("Stunning Strike", "Attack that stuns a single enemy", StunningStrike)
  def groundStrike: Card = mkCard("Ground Strike", "Slam the ground with your weapon, hitting all enemies and reducing their position", GroundStrike)

  def counter: Card = mkCard("Counter", "Attack that deals more damage the stronger the enemy", Counter)
  def destruction: Card = mkCard("Destruction", "Destructive attack that lowers an enemies defenses", Destruction)
}

trait Dexterity extends CardBuilder{
//  def quickAttack: Card = mkCard("Quick Attack", "Attack with reduced movement penalty", QuickAttack)
//  def assassinate: Card = mkCard("Assassinate", "Attack that deals more damage the more hp the enemy is missing", Assassinate)
//  def quickStep: Card = mkCard("Quick Step", "Player can take 2 turns", QuickStep)
//  def windStrike: Card = mkCard("Wind Strike", "Attacks all enemies", WindStrike)
//  def tripleStrike: Card = mkCard("Triple Strike", "Attacks an enemy three times", TripleStrike)
}

trait Utility extends CardBuilder {
  // Cost 100
  def miracle: Card = mkCard("Miracle", "Draw cards until your hand is full", Miracle)

  // Cost 70
  def rummage: Card = mkCard("Rummage", "Quickly search the area and draw 2 cards", Rummage)

  // Cost 0
  def replace: Card = mkCard("Replace", "Instantly replaces the player's hand (will draw at least 3 cards)", Replace)
  def restore: Card = mkCard("Restore", "Instantly places the top discarded card into your hand", Restore)
}

trait CampFire extends CardBuilder {
  def rest: Card = mkCard("Rest", "Rest until you are fully recovered", Rest)
  def explore: Card = mkCard("Explore", "Draw until your hand is full (always draw at least two cards)", Explore)
  def restAndExplore: Card = mkCard("Rest and Explore", "Recover some health and draw two cards (or one if full)", RestAndExplore)
}
