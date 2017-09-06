package chousen.game.cards

import java.util.UUID

import chousen.api.data._

object CardCatalogue extends Potions with PermanentEffects with Magic with Strength with Dexterity with Utility with CampFire with Equipment {

  // Deck built around stun/counter ST
  def fighterDeck: Seq[Card] = // 15
      Seq(
        healWounds, regen,
        rarePepe, rarePepe,                             // 4

        might, might, might, might,                                      // 10 potions
        haste, haste, haste, haste,
        flames, stoneSkin,

        elixirOfStrength, elixirOfDexterity,   // 2 elixirs

        essenceOfStrength, essenceOfStrength, essenceOfStrength, essenceOfStrength,  // 16 essences
        essenceOfStrength, essenceOfStrength, essenceOfStrength, essenceOfStrength,
        essenceOfStrength, essenceOfStrength,
        essenceOfDexterity, essenceOfDexterity, essenceOfDexterity, essenceOfDexterity,
        essenceOfDexterity, essenceOfDexterity,

        shortSword, broadsword, longSword,    // 5 equips
        chainmail, heavyArmour,

        groundStrike, groundStrike,       // 8 skills
        stunningStrike, stunningStrike,
        counter,
        crushingBlow,
        burningHammer,

        armoury, forgeArmour,                         // 14 cards
        trade, trade,

        rummage, rummage, rummage, rummage,
        refresh, refresh,
        miracle, miracle,
        manifestRage,
        increaseCharges,
        essenceBoost,
        reduceRequirements,
      )

  // Deck built around auto-attacks and rage
  def berserkerDeck: Seq[Card] = Seq(

    regen, regen, regen,
    rarePepe, rarePepe,                     // 7


    elixirOfStrength, elixirOfVitality,     // 2 el

    essenceOfStrength, essenceOfStrength, essenceOfStrength, essenceOfStrength,
    essenceOfStrength, essenceOfStrength, essenceOfStrength, essenceOfStrength,
    essenceOfStrength, essenceOfStrength,
    essenceOfVitality, essenceOfVitality, essenceOfVitality, essenceOfVitality, // 14 elix


    rage, rage, rage, rage,
    might, might, might, might,
    haste, haste, haste, haste,
    continuation, continuation, continuation, continuation,       // 16 pots
    trog, trog,
    lignification,

    mace, trollCrusher,               // 5 equip
    ringmail, orcishArmour,

    // 16 card
    armoury,

    rummage, rummage, rummage, rummage,
    trade, trade,
    essenceBoost,
    reduceRequirements, reduceRequirements,
    miracle, miracle, miracle, miracle,
    manifestRage, manifestRage
  )


  def rogueDeck: Seq[Card] = Seq(

    healWounds, healWounds,
    rarePepe, rarePepe, // 4

    might, might, might, might,   // 8
    haste, haste,
    stoneSkin,
    dexterity,

    elixirOfVitality, elixirOfIntelligence, // 2 exl

    essenceOfStrength, essenceOfStrength, essenceOfStrength, essenceOfStrength,
    essenceOfDexterity, essenceOfDexterity, essenceOfDexterity, essenceOfDexterity,
    essenceOfDexterity, essenceOfDexterity, essenceOfDexterity, essenceOfDexterity,
    essenceOfDexterity, essenceOfDexterity,
    essenceOfIntelligence, essenceOfIntelligence, essenceOfIntelligence,
    essenceOfVitality,                                    // 18 Essences


    quickStep, barrier,
    quickAttack, quickAttack,
    assassinate, assassinate,
    windStrike, windStrike, // 8 Abilities

    swordOfIntellect, ringmail,  // 2 equip

    armoury, armoury,          // 18 Card
    trade, trade,
    rummage, rummage, rummage, rummage,
    increaseCharges, increaseCharges, increaseCharges, increaseCharges,
    essenceBoost, essenceBoost,
    miracle, miracle,
    refresh, refresh
  )

  def tricksterDeck = Seq(
    essenceOfDexterity, essenceOfDexterity, essenceOfDexterity, essenceOfDexterity,
    essenceOfDexterity, essenceOfDexterity, essenceOfDexterity,
    essenceOfIntelligence, essenceOfIntelligence, essenceOfIntelligence, essenceOfIntelligence,
    essenceOfIntelligence, essenceOfIntelligence, essenceOfIntelligence,
    essenceOfVitality, essenceOfVitality, // 16 Essences


    elixirOfVitality, elixirOfVitality,  // 2 Elixirs

    haste, haste, haste, haste,           // 8 Potions
    stoneSkin, regen,
    flames,

    rarePepe, rarePepe,  // 2 Pepes

    pain, pain, pain,
    assassinate, assassinate, assassinate,
    quickStep, quickStep, quickStep, quickStep,
    windStrike, windStrike,
    lifeswap, lifeswap,// 12 Abilities

    daggerOfDavid, cape,                  // 2 Equipment

    miracle, miracle, miracle, miracle,   // 16 Card Actions
    rummage, rummage, rummage, rummage,
    increaseCharges, increaseCharges, increaseCharges, increaseCharges,
    recharge, recharge, recharge,
    trade,
    refresh
  )

  def mage = Seq(
    essenceOfIntelligence, essenceOfIntelligence, essenceOfIntelligence, essenceOfIntelligence,
    essenceOfIntelligence, essenceOfIntelligence, essenceOfIntelligence, essenceOfIntelligence,
    essenceOfIntelligence, essenceOfIntelligence, essenceOfIntelligence, essenceOfIntelligence,
    essenceOfVitality, essenceOfVitality, essenceOfVitality, // 15 Essences

    elixirOfIntelligence, elixirOfVitality,  // 2 Elixirs

    haste, haste, haste, haste,              // 8 Potions
    stoneSkin, stoneSkin, flames, flames,
    intelligence, intelligence,
    lignification,

    healWounds, rarePepe, rarePepe,  // 4 Pepes


    ember, ember, ember, ember,
    fireball, fireball, fireball,
    extinguish, extinguish,
    barrier, barrier,


    leatherArmour,                  // 2 Equipment

    miracle, miracle, miracle, miracle,   // 16 Card Actions
    rummage, rummage, rummage, rummage,
    increaseCharges, increaseCharges, increaseCharges, increaseCharges,
    recharge, recharge, recharge,
    refresh, refresh
  )


  def wizard: Seq[Card] = Seq(
    essenceOfIntelligence, essenceOfIntelligence, essenceOfIntelligence, essenceOfIntelligence,
    essenceOfIntelligence, essenceOfIntelligence, essenceOfIntelligence, essenceOfIntelligence,
    essenceOfIntelligence, essenceOfIntelligence, essenceOfIntelligence, essenceOfIntelligence,
    essenceOfVitality, essenceOfVitality, essenceOfVitality, essenceOfVitality, // 16 Essences

    elixirOfIntelligence, elixirOfVitality,  // 2 Elixirs

    haste, haste, haste, haste,              // 8 Potions
    stoneSkin, flames, flames,
    intelligence, intelligence,

    rarePepe, rarePepe,  // 4 Pepes

    magicMissile, magicMissile, magicMissile,    // 12 Abilities
    pain, pain,
    shatter, shatter,
    massDrain, massDrain, massDrain, massDrain,
    barrier, barrier,


    leatherArmour,                  // 2 Equipment

    miracle, miracle, miracle, miracle,   // 16 Card Actions
    rummage, rummage, rummage, rummage,
    increaseCharges, increaseCharges, increaseCharges, increaseCharges,
    recharge, recharge, recharge,
    refresh, refresh
  )



  //  def cheeseDeck: Seq[Card] = // 15
//    usefulCards ++
//      (4 of assassinate) ++ (4 of windStrike) ++ (4 of quickStep) ++
//      (4 of pain) ++ (4 of staticField) ++ (4 of fireball) ++
//      (4 of elixirOfDexterity) ++ (4 of elixirOfIntelligence) ++
//      restore.times(4) ++ replace.times(4) ++ miracle.times(4) ++ rummage.times(4)


  def passiveCards: Seq[Card] = Seq(rest, explore, restAndExplore, drop, destroy)
}

sealed trait CardBuilder {
  def mkCard(name: String, description: String, action: Action, charges:Int=0, requirements: Requirements=Requirements()) =
    Card(UUID.randomUUID(), name, description, action, if (charges == 0) None else Some(charges), if (charges == 0) None else Some(charges), requirements)

  def mkEquip(name: String, description: String, action: Action, requirements: Requirements=Requirements()) =
    Card(UUID.randomUUID(), name, description, action, None, None, requirements)
}

trait Potions extends CardBuilder {
  def haste: Card = Card(UUID.randomUUID(), "Potion of Haste", "Temporary increases player speed", Haste)
  def might: Card = Card(UUID.randomUUID(), "Potion of Might", "Temporary increases player strength", PotionOfMight)
  def intelligence: Card = Card(UUID.randomUUID(), "Potion of Intelligence", "Temporary increases player intelligence", PotionOfIntelligence)
  def stoneSkin: Card = Card(UUID.randomUUID(), "Stone Skin", "Temporary increases player defence", PotionOfStoneSkin)
  def dexterity: Card = Card(UUID.randomUUID(), "Potion of Dexterity", "Temporarily increases player dexterity", PotionOfDexterity)
  def rage: Card = Card(UUID.randomUUID(), "Potion of Rage", "Temporary increases health, damage, and speed", PotionOfRage)
  def trog: Card = Card(UUID.randomUUID(), "Potion of Trogg", "Consumes all Rage potions to give a massive temporary increase to health, damage, and speed", PotionOfTrogg)
  def continuation: Card = Card(UUID.randomUUID(), "Potion of Continuation", "Prolongs any temporary status effects", PotionOfContinuation)
  def regen: Card = Card(UUID.randomUUID(), "Potion of Regeneration", "Temporary increases life regeneration", PotionOfRegeneration)

  def flames: Card = mkCard("Potion of Flames", "Applies a long lasting Burn to all enemies", PotionOfFlames)
  def lignification: Card = mkCard("Potion of Lignification", "Turns the user into a slow Tree, increasing armour, strength, and regen.", PotionOfLignification)
}

trait PermanentEffects extends CardBuilder {
  def elixirOfStrength: Card = mkCard("Elixir of Strength ", "Permanently increases Strength by 2", ElixirOfStrength)
  def elixirOfDexterity: Card = mkCard("Elixir of Dexterity ", "Permanently increases Dexterity by 2", ElixirOfDexterity)
  def elixirOfIntelligence: Card = mkCard("Elixir of Intelligence ", "Permanently increases Intelligence by 2", ElixirOfIntelligence)
  def elixirOfVitality: Card = mkCard("Elixir of Vitality ", "Permanently increases Vitality by 2", ElixirOfVitality)
  def rarePepe: Card = mkCard("Rare Pepe ", "Gives the player a chunk of experience", RarePepe)

  def essenceOfStrength: Card = mkCard("Essence of Strength ", "Immediately increases Strength, only 1 essence may be played per turn", EssenceOfStrength)
  def essenceOfDexterity: Card = mkCard("Essence of Dexterity ", "Immediately increases Dexterity, only 1 essence may be played per turn", EssenceOfDexterity)
  def essenceOfIntelligence: Card = mkCard("Essence of Intelligence ", "Immediately increases Intelligence, only 1 essence may be played per turn", EssenceOfIntelligence)
  def essenceOfVitality: Card = mkCard("Essence of Vitality ", "Immediately increases Dexterity, only 1 essence may be played per turn", EssenceOfVitality)
}

trait Magic extends CardBuilder{
  def healWounds: Card = mkCard("Heal Wounds", "Heals 30-60HP depending on level", HealWounds, charges = 2)

  def fireball: Card = mkCard("Fireball", "Deals fire damage and burns to all enemies", Fireball, 2)
  def extinguish: Card = mkCard("Extinguish", "Extinguish burning enemies for huge damage", Extinguish, 3)
//  def staticField: Card = mkCard("Static Field", "Reduces all enemies hp by 33%", StaticField)
  def pain: Card = mkCard("Pain", "Reduces the hp of a single target by around 50%", Pain, 3)
  def shatter: Card = mkCard("Shatter", "Reduce player to 1hp and deal the same damage to all enemies", Shatter, 1)
  def magicMissile: Card = mkCard("Magic Missile", "Deals high magic damage to a single enemy", MagicMissile, 4)
  def ember: Card = mkCard("Ember", "Burn a single target with fire magic", Ember, 4)
  def barrier: Card = mkCard("Barrier", "Creates a magic barrier to protect the user", Barrier, 3)
  def lifeswap: Card = mkCard("Life Swap", "Swap's the health of the player with an enemy", LifeSwap, 1)
  def massDrain: Card = mkCard("Mass Drain", "Drains health from multiple enemies and heals the player", MassDrain, 3)
}

trait Strength extends CardBuilder{
  def crushingBlow: Card = mkCard("Crushing Blow", "Deals heavy damage, but has an increased movement penalty", CrushingBlow, 4)
  def burningHammer: Card = mkCard("Burning Hammer", "Attack that applies the burn status to an enemy, deals additional damage to burning enemies", BurningHammer, 4)

  def stunningStrike: Card = mkCard("Stunning Strike", "Attack that stuns and reduces the speed of a single enemy", StunningStrike, 3)
  def groundStrike: Card = mkCard("Ground Strike", "Slam the ground with your weapon, hitting all enemies and reducing their position", GroundStrike, 2)

  def counter: Card = mkCard("Counter", "Attack that deals more damage the more strength an enemy has", Counter, 2)
  def destruction: Card = mkCard("Destruction", "Destructive attack that lowers an enemies vitality", Destruction, 4)
}

trait Dexterity extends CardBuilder{
  def quickAttack: Card = mkCard("Quick Attack", "Attack with reduced movement penalty", QuickAttack, 4)
  def assassinate: Card = mkCard("Assassinate", "Attack that deals more damage the more hp the enemy is missing", Assassinate, 3)
  def quickStep: Card = mkCard("Quick Step", "Increases the Player's position (affected by Dexterity)", QuickStep, 2)
  def windStrike: Card = mkCard("Wind Strike", "Attacks all enemies", WindStrike, 3)
//  def tripleStrike: Card = mkCard("Triple Strike", "Attacks an enemy three times", TripleStrike)
}

trait Utility extends CardBuilder {

  // Hand size limited
  def miracle: Card = mkCard("Miracle", "Draw cards until your hand is full", Miracle)

  // Not limited
  def rummage: Card = mkCard("Rummage", "Quickly search the area and always draw 2 cards (no hand limit)", Rummage)
  def replace: Card = mkCard("Replace", "Instantly replaces the player's hand (will draw at least 3 cards)", Replace)
  def restore: Card = mkCard("Restore", "Instantly places the top discarded card into your hand", Restore)

  def trade: Card = mkCard("Trade", "Discard one card and draw 3 cards", Trade)

  // Require discard
  def forgeWeapon: Card = mkCard("Forge Weapon", "Discard one card and place the next weapon in your deck in your hand", ForgeWeapon)
  def forgeArmour: Card = mkCard("Forge Armour", "Discard one card and place the next armour in your deck in your hand", ForgeArmour)
  def manifestRage: Card = mkCard("Manifest Rage", "Discard one card. Place an additional Potion of Rage to your hand and deck", ManifestRage)
  def essenceBoost: Card = mkCard("Essence Boost", "Discard one card. Draw essences from your deck until your hand is full", EssenceBoost)
  def recharge: Card = mkCard("Recharge", "Recharges all charges of all ability cards in your hand", Recharge)


  // def randomDiscovery: Card = mkCard("Random Discovery", "Choose a card and place on top of the deck", RandomDiscovery)
  def refresh: Card = mkCard("Refresh", "Discard all non-Ability cards, draw 4 cards", Refresh)
  def armoury: Card = mkCard("Armoury", "Move the first 2 Equip cards in your deck to the top of the deck", Armoury)

  // Require target
  def reduceRequirements: Card = mkCard("Reduce Requirements", "Reduces all requirements for the chosen card by 5", ReduceRequirements)
  def increaseCharges: Card = mkCard("Increase Charges", "Increases the number of charges of an ability card by 2", IncreaseCharges)
}

trait CampFire extends CardBuilder {
  def rest: Card = mkCard("Rest", "Rest until you are fully recovered", Rest)
  def explore: Card = mkCard("Explore", "Draw until your hand is full (always draw at least two cards)", Explore)
  def restAndExplore: Card = mkCard("Rest and Explore", "Recover some health and draw two cards (or one if full)", RestAndExplore)
  def drop: Card = mkCard("Drop", "Discard an item by the Camp Fire", Drop)
  def destroy: Card = mkCard("Destroy", "Destroy an item in the fire", Destroy)
}

trait Equipment extends CardBuilder {
  def club: Card = mkEquip("Club", "Generic Club, minimal increase to damage",
    Club)
  def shortSword: Card = mkEquip("Short Sword", "Generic sword, slight increase to damage",
    ShortSword, Requirements(str = Some(10), dex = Some(9)))
  def mace: Card = mkEquip("Mace", "Slight increase to damage",
    Mace, Requirements(str = Some(12)))
  def broadsword: Card = mkEquip("Broadsword", "Moderate increase to damage",
    BroadSword, Requirements(str = Some(16), dex = Some(11)))
  def giantClub: Card = mkEquip("Giant Club", "Heavy increase to damage",
    GiantClub, Requirements(str = Some(19)))
  def longSword: Card = mkEquip("Long Sword", "Heavy increase to damage",
    LongSword, Requirements(str = Some(17), dex = Some(13)))


  def trollCrusher: Card = mkEquip("Troll Crusher", "Moderate increase to damage. Bonus damage based on the enemies current HP",
    TrollCrusher, Requirements(str = Some(22)))
  def swordOfIntellect: Card = mkEquip("Sword of Intellect", "Slight increase to damage. Intellect affects attack damage",
    SwordOfIntellect, Requirements(str = Some(13), dex = Some(13)))
  def daggerOfDavid: Card = mkEquip("Dagger of David", "No increase to damage. Deal bonus damage based on the enemies max HP.",
    DaggerOfDavid)

  def cape: Card = mkEquip("Cape", "Shiny red cape, quick to equip, has a minimal effect on damage taken",
    Cape, Requirements())
  def leatherArmour: Card = mkEquip("Leather Armour", "Generic armour, slightly reduces damage taken",
    LeatherArmour, Requirements(str = Some(8)))
  def ringmail: Card = mkEquip("Ringmail", "Generic armour, slightly reduces damage taken",
    Ringmail, Requirements(str = Some(10)))
  def chainmail: Card = mkEquip("Chainmail", "Generic armour, moderately reduces damage taken",
    Chainmail, Requirements(str = Some(16)))
  def heavyArmour: Card = mkEquip("Heavy Armour", "Generic armour, heavily reduces damage taken",
    HeavyArmour, Requirements(str = Some(22)))
  def orcishArmour: Card = mkEquip("Orcish Armour", "Orc armour, heavily reduces damage taken",
    OrcishArmour, Requirements(str = Some(24)))

}
