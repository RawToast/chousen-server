package chousen.game.actions

import java.util.UUID

import chousen.Optics._
import chousen.api.data._
import chousen.game.core.turn.PositionCalculator
import chousen.util.LensUtil

class EquipmentActionHandler {

  def handle(action: EquipAction, uuid: UUID): (GameState) => GameState = {
    LensUtil.duoLens(PlayerLens, MessagesLens).modify{
      case (p:Player, msgs: Seq[GameMessage]) =>
        actions(action)(p, msgs, uuid)
    }
  }

  private def actions(action: EquipAction): (Player, Seq[GameMessage], UUID) => (Player, Seq[GameMessage]) = {
    action match {
      case Club => club
      case ShortSword => shortSword
      case Mace => mace
      case BroadSword => boardSword
      case Longsword => kodachi
      case GiantClub => giantClub

      case TrollCrusher => trollCrusher
      case SwordOfIntellect => swordOfIntellect
      case DaggerOfDavid => daggerOfDavid
      case QuickBlade => quickBlade

      case Cape => cape
      case LeatherArmour => leatherArmour
      case Ringmail => ringmail
      case Chainmail => chainmail
      case HeavyArmour => heavyArmour
      case OrcishArmour => orcArmour

      // Treasure
      case RedCape => redCape
      case MagePlate => magePlate
      case RoyalChainmail => royalchain
      case RenartsDeceiver => rensDeceiver
      case Manamune => manamune
      case TroggsAnnihilator => troggsAnni
      case WandOfDefiance => wandOfDef
      case GreatSword => greatSword
    }
  }

  def weapon(name: String, dmg: Int, effects: Seq[WeaponEffect]= Seq.empty) = (p: Player, msgs: Seq[GameMessage], uuid: UUID) => {

    val message = GameMessage(s"${p.name} equips $name.")

    val lens = PlayerWeaponLens.set(Option(Weapon(uuid, name, dmg, effects = effects)))
      .andThen(PositionCalculator.calculatePosition(_: Player, cost = PositionCalculator.ENHANCED))

    lens.apply(p) -> (msgs :+ message)
  }

  def club(p: Player, msgs: Seq[GameMessage], uuid: UUID): (Player, Seq[GameMessage]) =
    weapon("Club", 3)(p, msgs, uuid)

  def shortSword(p: Player, msgs: Seq[GameMessage], uuid: UUID): (Player, Seq[GameMessage]) =
    weapon("Short Sword", 6)(p, msgs, uuid)

  def mace(p: Player, msgs: Seq[GameMessage], uuid: UUID): (Player, Seq[GameMessage]) =
    weapon("Mace", 6)(p, msgs, uuid)

  def boardSword(p: Player, msgs: Seq[GameMessage], uuid: UUID): (Player, Seq[GameMessage]) =
    weapon("Broadsword", 10)(p, msgs, uuid)

  def kodachi(p: Player, msgs: Seq[GameMessage], uuid: UUID): (Player, Seq[GameMessage]) =
    weapon("Long Sword", 15)(p, msgs, uuid)

  def giantClub(p: Player, msgs: Seq[GameMessage], uuid: UUID): (Player, Seq[GameMessage]) =
    weapon("Giant Club", 16)(p, msgs, uuid)

  def trollCrusher(p: Player, msgs: Seq[GameMessage], uuid: UUID): (Player, Seq[GameMessage]) =
    weapon("Troll Crusher", 9, Seq(Crush))(p, msgs, uuid)

  def swordOfIntellect(p: Player, msgs: Seq[GameMessage], uuid: UUID): (Player, Seq[GameMessage]) =
    weapon("Sword of Intellect", 6, Seq(Magic))(p, msgs, uuid)

  def daggerOfDavid(p: Player, msgs: Seq[GameMessage], uuid: UUID): (Player, Seq[GameMessage]) =
    weapon("Dagger of David", 0, Seq(Deadly))(p, msgs, uuid)

  def quickBlade(p: Player, msgs: Seq[GameMessage], uuid: UUID): (Player, Seq[GameMessage]) =
    weapon("Quick Blade", 9, Seq(Quick))(p, msgs, uuid)


  // Armour
  def cape(p: Player, msgs: Seq[GameMessage], uuid: UUID): (Player, Seq[GameMessage]) = {
    armour("Cape", 2, pen = PositionCalculator.FAST)(p, msgs, uuid)
  }

  def leatherArmour(p: Player, msgs: Seq[GameMessage], uuid: UUID): (Player, Seq[GameMessage]) = {
    armour("Leather Armour", 4)(p, msgs, uuid)
  }

  def ringmail(p: Player, msgs: Seq[GameMessage], uuid: UUID): (Player, Seq[GameMessage]) = {
    armour("Ringmail", 6)(p, msgs, uuid)
  }

  def chainmail(p: Player, msgs: Seq[GameMessage], uuid: UUID): (Player, Seq[GameMessage]) = {
    armour("Chainmail", 10)(p, msgs, uuid)
  }

  def heavyArmour(p: Player, msgs: Seq[GameMessage], uuid: UUID): (Player, Seq[GameMessage]) = {
    armour("Heavy Armour", 16)(p, msgs, uuid)
  }


  //Treasure Only
  def redCape(p: Player, msgs: Seq[GameMessage], uuid: UUID): (Player, Seq[GameMessage]) = {
    armour("Red Cape", 5, pen = PositionCalculator.FAST)(p, msgs, uuid)
  }


  def magePlate(p: Player, msgs: Seq[GameMessage], uuid: UUID): (Player, Seq[GameMessage]) = {
    armour("Mage Plate", 9)(p, msgs, uuid)
  }

  def royalchain(p: Player, msgs: Seq[GameMessage], uuid: UUID): (Player, Seq[GameMessage]) = {
    armour("Royal Chainmail", 15)(p, msgs, uuid)
  }

  def orcArmour(p: Player, msgs: Seq[GameMessage], uuid: UUID): (Player, Seq[GameMessage]) = {
    armour("Orcish Armour", 21)(p, msgs, uuid)
  }


  def rensDeceiver(p: Player, msgs: Seq[GameMessage], uuid: UUID): (Player, Seq[GameMessage]) =
    weapon("Renart's Deciever", 10, Seq(Quick, Deceive))(p, msgs, uuid)

  def troggsAnni(p: Player, msgs: Seq[GameMessage], uuid: UUID): (Player, Seq[GameMessage]) =
    weapon("Trogg's Annihilator", 11, Seq(Deadly, Maim))(p, msgs, uuid)

  def greatSword(p: Player, msgs: Seq[GameMessage], uuid: UUID): (Player, Seq[GameMessage]) =
    weapon("Great Sword", 9, Seq(Heavy, Heavy))(p, msgs, uuid)

  def manamune(p: Player, msgs: Seq[GameMessage], uuid: UUID): (Player, Seq[GameMessage]) =
    weapon("Manamune", 3, Seq(Magic, Magic))(p, msgs, uuid)

  def wandOfDef(p: Player, msgs: Seq[GameMessage], uuid: UUID): (Player, Seq[GameMessage]) =
    weapon("Defiant Wand", 4, Seq(Protection))(p, msgs, uuid)

  private def armour(name: String, ac: Int, pen:Int = PositionCalculator.TURTLE) = (p: Player, msgs: Seq[GameMessage], uuid: UUID) => {
    val message = GameMessage(s"${p.name} puts on $name.")

    val lens = PlayerArmourLens.set(Option(Armour(uuid, name, ac)))
      .andThen(PositionCalculator.calculatePosition(_: Player, cost = pen))

    lens.apply(p) -> (msgs :+ message)
  }

}
