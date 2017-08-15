package chousen.game.cards

import java.util.UUID

import chousen.api.data._

object CardCatalogue extends Potions with PermanentEffects with Magic with Strength with Dexterity with Utility with CampFire with Equipment {


  def defaultDeck: Seq[Card] = // 15
      Seq(healWounds, healWounds, healWounds, healWounds,
        rarePepe, rarePepe, rarePepe, rarePepe,

        might, might, might, might,
        haste, haste, haste, haste,
        stoneSkin, stoneSkin,
        rage, rage,
        continuation, continuation,

        elixirOfStrength, elixirOfStrength, elixirOfStrength, elixirOfStrength,
        elixirOfVitality, elixirOfVitality,

        broadsword, giantClub, swordOfIntellect, chainmail,

        groundStrike, groundStrike, groundStrike,

        crushingBlow, crushingBlow,
        stunningStrike, stunningStrike,
        counter, counter,
        destruction, destruction,

        rummage, rummage, rummage, rummage,
        replace, replace,
        restore, restore,
        miracle, miracle
      )

//  // Deck built around auto-attacks and rage
//  def berserkerDeck = {
//
//    healWounds, healWounds, healWounds, healWounds,
//      /* regen, regen, regen, regen, */
//    rarePepe, rarePepe, rarePepe, rarePepe,
//
//    elixirOfStrength, elixirOfStrength, elixirOfStrength, elixirOfStrength,
//    elixirOfDexterity, elixirOfDexterity, elixirOfVitality, elixirOfVitality,
//
//
//    rage, rage, rage, rage§
//
//
//    broadsword, giantClub, chainmail, chainmail,
//
//    rummage, rummage, rummage, rummage,
//    replace, replace
//  }


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
  def mkCard(name: String, description: String, action: Action, charges:Int=0) =
    Card(UUID.randomUUID(), name, description, action, if (charges == 0) None else Some(charges))
}

trait Potions extends CardBuilder {
  def healWounds: Card = mkCard("Heal Wounds", "Heals around 30HP", HealWounds)
  def haste: Card = Card(UUID.randomUUID(), "Potion of Haste", "Temporarily increases player speed", Haste)
  def might: Card = Card(UUID.randomUUID(), "Potion of Might", "Temporarily increases player strength", PotionOfMight)
//  def intelligence: Card = Card(UUID.randomUUID(), "Potion of Intelligence", "Temporarily increases player intelligence", PotionOfIntelligence)
  def stoneSkin: Card = Card(UUID.randomUUID(), "Stone Skin", "Temporarily increases player defence", PotionOfStoneSkin)
//  def dexterity: Card = Card(UUID.randomUUID(), "Potion of Dexterity", "Temporarily increases player dexterity", PotionOfDexterity)
  def rage: Card = Card(UUID.randomUUID(), "Potion of Rage", "Temporarily increases health, damage, and speed", PotionOfRage)
  def continuation: Card = Card(UUID.randomUUID(), "Potion of Continuation", "Prolongs any temporary status effects", PotionOfContinuation)
}

trait PermanentEffects extends CardBuilder {
  def elixirOfStrength: Card = mkCard("Elixir of Strength ", "Permanently increases Strength by 2", ElixirOfStrength)
  def elixirOfDexterity: Card = mkCard("Elixir of Dexterity ", "Permanently increases Dexterity by 2", ElixirOfDexterity)
//  def elixirOfIntelligence: Card = mkCard("Elixir of Intelligence ", "Permanently increases Intelligence by 2", ElixirOfIntelligence)
  def elixirOfVitality: Card = mkCard("Elixir of Vitality ", "Permanently increases Vitality by 2", ElixirOfVitality)
  def rarePepe: Card = mkCard("Rare Pepe ", "Gives the player a chunk of experience", RarePepe)
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
  def crushingBlow: Card = mkCard("Crushing Blow", "Deals heavy damage, but has an increased movement penalty", CrushingBlow, 4)
  def stunningStrike: Card = mkCard("Stunning Strike", "Attack that stuns and reduces the speed of a single enemy", StunningStrike, 4)
  def groundStrike: Card = mkCard("Ground Strike", "Slam the ground with your weapon, hitting all enemies and reducing their position", GroundStrike, 2)

  def counter: Card = mkCard("Counter", "Attack that deals more damage the stronger the enemy", Counter, 2)
  def destruction: Card = mkCard("Destruction", "Destructive attack that lowers an enemies defenses", Destruction, 4)
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

trait Equipment extends CardBuilder {
  def broadsword: Card = mkCard("Broadsword", "Generic Broadsword, significant increase to damage", BroadSword)
  def giantClub: Card = mkCard("Giant Club", "Giant Club, increases damage and deals additional damage based on the enemies current HP", GiantClub)
  def swordOfIntellect: Card = mkCard("Sword of Intellect", "Sword of Intellect, increases damage and applies Intellect to attack damage", SwordOfIntellect)

  def chainmail: Card = mkCard("Chainmail", "Generic armour, reduces damage taken", Chainmail)
}
