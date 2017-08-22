package chousen.game.actions

import java.util.UUID

import chousen.Optics._
import chousen.api.data._
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
      case GiantClub => giantClub
      case TrollCrusher => trollCrusher

      case SwordOfIntellect => swordOfIntellect
      case LeatherArmour => leatherArmour
      case Ringmail => ringmail
      case Chainmail => chainmail
      case HeavyArmour => heavyArmour
      case OrcishArmour => orcArmour
    }
  }

  def club(p: Player, msgs: Seq[GameMessage], uuid: UUID): (Player, Seq[GameMessage]) =
    weapon("Club", 3)(p, msgs, uuid)

  def shortSword(p: Player, msgs: Seq[GameMessage], uuid: UUID): (Player, Seq[GameMessage]) =
    weapon("Giant Club", 6)(p, msgs, uuid)

  def mace(p: Player, msgs: Seq[GameMessage], uuid: UUID): (Player, Seq[GameMessage]) =
    weapon("Mace", 6)(p, msgs, uuid)

  def boardSword(p: Player, msgs: Seq[GameMessage], uuid: UUID): (Player, Seq[GameMessage]) =
    weapon("Broadsword", 10)(p, msgs, uuid)

  def giantClub(p: Player, msgs: Seq[GameMessage], uuid: UUID): (Player, Seq[GameMessage]) =
    weapon("Giant Club", 8, Seq(Crush))(p, msgs, uuid)

  def trollCrusher(p: Player, msgs: Seq[GameMessage], uuid: UUID): (Player, Seq[GameMessage]) =
    weapon("Troll Crusher", 16)(p, msgs, uuid)

  def swordOfIntellect(p: Player, msgs: Seq[GameMessage], uuid: UUID): (Player, Seq[GameMessage]) =
    weapon("Sword of Intellect", 1, Seq(Magic))(p, msgs, uuid)

  def weapon(name: String, dmg: Int, effects: Seq[WeaponEffect]= Seq.empty) = (p: Player, msgs: Seq[GameMessage], uuid: UUID) => {

    val message = GameMessage(s"${p.name} equips $name.")

    val lens = PlayerWeaponLens.set(Option(Weapon(uuid, name, dmg, effects = effects)))
      .andThen(PlayerPositionLens.modify(p => p - 100))

    lens.apply(p) -> (msgs :+ message)
  }



  // Armour
  def leatherArmour(p: Player, msgs: Seq[GameMessage], uuid: UUID): (Player, Seq[GameMessage]) = {
    armour("Leather Armour", 2)(p, msgs, uuid)
  }

  def ringmail(p: Player, msgs: Seq[GameMessage], uuid: UUID): (Player, Seq[GameMessage]) = {
    armour("Ringmail", 4)(p, msgs, uuid)
  }

  def chainmail(p: Player, msgs: Seq[GameMessage], uuid: UUID): (Player, Seq[GameMessage]) = {
    armour("Chainmail", 7)(p, msgs, uuid)
  }

  def heavyArmour(p: Player, msgs: Seq[GameMessage], uuid: UUID): (Player, Seq[GameMessage]) = {
    armour("Heavy Armour", 10)(p, msgs, uuid)
  }

  def orcArmour(p: Player, msgs: Seq[GameMessage], uuid: UUID): (Player, Seq[GameMessage]) = {
    armour("Orcish Armour", 11)(p, msgs, uuid)
  }

  private def armour(name: String, ac: Int) = (p: Player, msgs: Seq[GameMessage], uuid: UUID) => {
    val message = GameMessage(s"${p.name} puts on $name.")

    val lens = PlayerArmourLens.set(Option(Armour(uuid, name, ac)))
      .andThen(PlayerPositionLens.modify(p => p - 200))

    lens.apply(p) -> (msgs :+ message)
  }

}
