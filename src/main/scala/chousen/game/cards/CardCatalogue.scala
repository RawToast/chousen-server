package chousen.game.cards

import java.util.UUID

import chousen.api.data._

object CardCatalogue extends Potions with PermanentEffects with Magic with Strength with Dexterity with Utility with CampFire with Equipment {

  // Deck built around stun/counter ST
  def fighterDeck: Seq[Card] = // 15
      Seq(
        healWounds, healWounds,
        rarePepe, rarePepe, rarePepe, rarePepe,

        might, might, might, might,
        haste, haste, haste, haste,
        stoneSkin, stoneSkin,
        regen, regen,

        elixirOfStrength, elixirOfStrength, elixirOfDexterity, elixirOfVitality,

        essenceOfStrength, essenceOfStrength, essenceOfStrength, essenceOfStrength,
        essenceOfStrength, essenceOfStrength, essenceOfStrength, essenceOfStrength,
        essenceOfDexterity, essenceOfDexterity, essenceOfVitality, essenceOfVitality,

        club, shortSword, broadsword, swordOfIntellect,
        ringmail, chainmail, heavyArmour,

        groundStrike, groundStrike, groundStrike,

        stunningStrike, stunningStrike,
        counter, counter,

        forgeWeapon, forgeArmour, trade, trade,

        rummage, rummage, rummage, rummage,
        replace, replace,
        miracle, miracle
      )

  // Deck built around auto-attacks and rage
  def berserkerDeck: Seq[Card] = Seq(

    healWounds,
    regen, regen, regen, regen,
    rarePepe, rarePepe, rarePepe, rarePepe,


    elixirOfStrength, elixirOfStrength, elixirOfDexterity, elixirOfVitality,

    essenceOfStrength, essenceOfStrength, essenceOfStrength, essenceOfStrength,
    essenceOfStrength, essenceOfStrength, essenceOfStrength, essenceOfStrength,
    essenceOfVitality, essenceOfVitality, essenceOfVitality, essenceOfVitality,


    rage, rage, rage, rage,
    might, might, might, might,
    haste, haste, haste, haste,
    continuation, continuation, continuation, continuation,
    stoneSkin, stoneSkin,

    // GiantClub is better for this build -- no other access to % damage
    mace, giantClub, giantClub,
    leatherArmour, chainmail, heavyArmour,

    groundStrike,

    forgeWeapon, forgeArmour, trade, trade,

    rummage, rummage,
    restore, restore, miracle, miracle,
  )

  // Deck built around high strength skills
  def warriorDeck: Seq[Card] = Seq(

    healWounds, healWounds,
    regen, regen,
    rarePepe, rarePepe, rarePepe, rarePepe,

    elixirOfStrength, elixirOfStrength, elixirOfStrength, elixirOfVitality,

    essenceOfStrength, essenceOfStrength, essenceOfStrength, essenceOfStrength,
    essenceOfStrength, essenceOfStrength, essenceOfStrength, essenceOfStrength,
    essenceOfStrength, essenceOfStrength, essenceOfVitality, essenceOfVitality,

    might, might, might, might,
    haste, haste, haste, haste,
    stoneSkin, continuation,

    mace, giantClub, trollCrusher,
    chainmail, heavyArmour, orcishArmour,

    destruction, destruction, destruction, destruction,
    crushingBlow, crushingBlow, crushingBlow, crushingBlow,

    groundStrike, groundStrike,

    forgeWeapon, forgeArmour, trade, trade,

    rummage, rummage, rummage, rummage,
    miracle, miracle
  )


//  def cheeseDeck: Seq[Card] = // 15
//    usefulCards ++
//      (4 of assassinate) ++ (4 of windStrike) ++ (4 of quickStep) ++
//      (4 of pain) ++ (4 of staticField) ++ (4 of fireball) ++
//      (4 of elixirOfDexterity) ++ (4 of elixirOfIntelligence) ++
//      restore.times(4) ++ replace.times(4) ++ miracle.times(4) ++ rummage.times(4)


  def passiveCards: Seq[Card] = Seq(rest, explore, restAndExplore, drop)
}

sealed trait CardBuilder {
  def mkCard(name: String, description: String, action: Action, charges:Int=0, requirements: Requirements=Requirements()) =
    Card(UUID.randomUUID(), name, description, action, if (charges == 0) None else Some(charges), if (charges == 0) None else Some(charges), requirements)

  def mkEquip(name: String, description: String, action: Action, requirements: Requirements=Requirements()) =
    Card(UUID.randomUUID(), name, description, action, None, None, requirements)
}

trait Potions extends CardBuilder {
  def healWounds: Card = mkCard("Heal Wounds", "Heals around 30HP", HealWounds, charges = 2)
  def haste: Card = Card(UUID.randomUUID(), "Potion of Haste", "Temporarily increases player speed", Haste)
  def might: Card = Card(UUID.randomUUID(), "Potion of Might", "Temporarily increases player strength", PotionOfMight)
//  def intelligence: Card = Card(UUID.randomUUID(), "Potion of Intelligence", "Temporarily increases player intelligence", PotionOfIntelligence)
  def stoneSkin: Card = Card(UUID.randomUUID(), "Stone Skin", "Temporarily increases player defence", PotionOfStoneSkin)
//  def dexterity: Card = Card(UUID.randomUUID(), "Potion of Dexterity", "Temporarily increases player dexterity", PotionOfDexterity)
  def rage: Card = Card(UUID.randomUUID(), "Potion of Rage", "Temporarily increases health, damage, and speed", PotionOfRage)
  def continuation: Card = Card(UUID.randomUUID(), "Potion of Continuation", "Prolongs any temporary status effects", PotionOfContinuation)
  def regen: Card = Card(UUID.randomUUID(), "Potion of Regeneration", "Temporarily increases life regeneration", PotionOfRegeneration)
}

trait PermanentEffects extends CardBuilder {
  def elixirOfStrength: Card = mkCard("Elixir of Strength ", "Permanently increases Strength by 2", ElixirOfStrength)
  def elixirOfDexterity: Card = mkCard("Elixir of Dexterity ", "Permanently increases Dexterity by 2", ElixirOfDexterity)
//  def elixirOfIntelligence: Card = mkCard("Elixir of Intelligence ", "Permanently increases Intelligence by 2", ElixirOfIntelligence)
  def elixirOfVitality: Card = mkCard("Elixir of Vitality ", "Permanently increases Vitality by 2", ElixirOfVitality)
  def rarePepe: Card = mkCard("Rare Pepe ", "Gives the player a chunk of experience", RarePepe)

  def essenceOfStrength: Card = mkCard("Essence of Strength ", "Immediately increases Strength, only 1 essence may be played per turn", EssenceOfStrength)
  def essenceOfDexterity: Card = mkCard("Essence of Dexterity ", "Immediately increases Dexterity, only 1 essence may be played per turn", EssenceOfDexterity)
  //  def essenceOfIntelligence: Card = mkCard("Essence of Intelligence ", "Immediately increases Intelligence, only 1 essence may be played per turn", EssenceOfIntelligence)
  def essenceOfVitality: Card = mkCard("Essence of Vitality ", "Immediately increases Dexterity, only 1 essence may be played per turn", EssenceOfVitality)
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
  def stunningStrike: Card = mkCard("Stunning Strike", "Attack that stuns and reduces the speed of a single enemy", StunningStrike, 3)
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
  def rummage: Card = mkCard("Rummage", "Quickly search the area and always draw 2 cards (no hand limit)", Rummage)
  def trade: Card = mkCard("Trade", "Discard one card and draw up to 3 cards, stop if the hand limit is reached)", Trade)


  // Cost 0
  def replace: Card = mkCard("Replace", "Instantly replaces the player's hand (will draw at least 3 cards)", Replace)
  def restore: Card = mkCard("Restore", "Instantly places the top discarded card into your hand", Restore)

  def forgeWeapon: Card = mkCard("Forge Weapon", "Discard one card and place the next weapon in your deck in your hand", ForgeWeapon)
  def forgeArmour: Card = mkCard("Forge Armour", "Discard one card and place the next armour in your deck in your hand", ForgeArmour)
}

trait CampFire extends CardBuilder {
  def rest: Card = mkCard("Rest", "Rest until you are fully recovered", Rest)
  def explore: Card = mkCard("Explore", "Draw until your hand is full (always draw at least two cards)", Explore)
  def restAndExplore: Card = mkCard("Rest and Explore", "Recover some health and draw two cards (or one if full)", RestAndExplore)
  def drop: Card = mkCard("Drop", "Drop an item by the Camp Fire", Drop)
}

trait Equipment extends CardBuilder {
  def club: Card = mkEquip("Club", "Generic Club, minimal increase to damage",
    Club)
  def shortSword: Card = mkEquip("Short Sword", "Generic sword, slight increase to damage",
    ShortSword, Requirements(str = Some(10), dex = Some(9)))
  def mace: Card = mkEquip("Mace", "Generic mace, slight increase to damage",
    Mace, Requirements(str = Some(12)))
  def broadsword: Card = mkEquip("Broadsword", "Broadsword, moderate increase to damage",
    BroadSword, Requirements(str = Some(16), dex = Some(11)))
  def giantClub: Card = mkEquip("Giant Club", "Giant Club, moderate damage and deals bonus damage based on the enemies current HP",
    GiantClub, Requirements(str = Some(20)))
  def trollCrusher: Card = mkEquip("Troll Crusher", "Troll Crusher, heavy increase to damage",
    TrollCrusher, Requirements(str = Some(22)))

  def swordOfIntellect: Card = mkEquip("Sword of Intellect", "Sword of Intellect, minimal increase to damage but applies Intellect to attack damage",
    SwordOfIntellect, Requirements(str = Some(13), dex = Some(13)))

  def leatherArmour: Card = mkEquip("Leather Armour", "Generic armour, has a minimal effect on damage taken",
    LeatherArmour, Requirements())
  def ringmail: Card = mkEquip("Ringmail", "Generic armour, slightly reduces damage taken",
    Ringmail, Requirements(str = Some(10)))
  def chainmail: Card = mkEquip("Chainmail", "Generic armour, moderately reduces damage taken",
    Chainmail, Requirements(str = Some(16)))
  def heavyArmour: Card = mkEquip("Heavy Armour", "Generic armour, heavily reduces damage taken",
    HeavyArmour, Requirements(str = Some(22)))
  def orcishArmour: Card = mkEquip("Orcish Armour", "Orc armour, heavily reduces damage taken",
    HeavyArmour, Requirements(str = Some(24)))
}
