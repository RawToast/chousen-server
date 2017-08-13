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
      case GiantClub => giantClub
      case BroadSword => boardSword
      case SwordOfIntellect => swordOfIntellect
      case Chainmail => chainmail
    }
  }

  def giantClub(p: Player, msgs: Seq[GameMessage], uuid: UUID): (Player, Seq[GameMessage]) = {

    val message = GameMessage(s"${p.name} equips a Giant Club.")

    val lens = PlayerWeaponLens.set(Option(Weapon(uuid, "Giant Club", 5, Requirements(), Seq(Crush))))
      .andThen(PlayerPositionLens.modify(p => p - 200))

    lens.apply(p) -> (msgs :+ message)
  }


  def boardSword(p: Player, msgs: Seq[GameMessage], uuid: UUID): (Player, Seq[GameMessage]) = {

    val message = GameMessage(s"${p.name} equips a Broad Sword.")

    val lens = PlayerWeaponLens.set(Option(Weapon(uuid, "Broad Sword", 10, Requirements())))
      .andThen(PlayerPositionLens.modify(p => p - 200))

    lens.apply(p) -> (msgs :+ message)
  }

  def swordOfIntellect(p: Player, msgs: Seq[GameMessage], uuid: UUID): (Player, Seq[GameMessage]) = {

    val message = GameMessage(s"${p.name} equips the Sword of Intellect.")

    val lens = PlayerWeaponLens.set(Option(Weapon(uuid, "Sword of Intellect", 2, Requirements(), Seq(Magic))))
      .andThen(PlayerPositionLens.modify(p => p - 200))

    lens.apply(p) -> (msgs :+ message)
  }



  // Armour

  def chainmail(p: Player, msgs: Seq[GameMessage], uuid: UUID): (Player, Seq[GameMessage]) = {

    val message = GameMessage(s"${p.name} puts on some chainmail.")

    val lens = PlayerArmourLens.set(Option(Armour(uuid, "Chainmail", 8)))
      .andThen(PlayerPositionLens.modify(p => p - 200))

    lens.apply(p) -> (msgs :+ message)
  }



}
