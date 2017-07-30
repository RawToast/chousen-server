package chousen.game.status

import chousen.Optics._
import chousen.api.data._
import monocle.Lens

class StatusCalculator {
  def calculate(p: Player): Player = {

    if (p.status.isEmpty) p
    else calcStatusEffects(p)
  }


  private def calcStatusEffects(player: Player): Player = {

    val status: Seq[Status] = player.status

    status.foldLeft(player)(foldStatus)
  }

  private def foldStatus(p: Player, s: Status): Player = {
    val func = s.effect match {
      case Fast => fast
      case StoneSkin => stoneskin
      case Might => might
      case Dexterity => dexterity
      case Smart => smart
    }
    func(p, s)
  }

  private def fast = (p: Player, s: Status) =>
    doSmt(PlayerSpeedLens)(i => i + s.amount.getOrElse(0))(p)

  private def stoneskin = (p: Player, s: Status) =>
    doSmt(PlayerVitalityLens)(i => i + s.amount.getOrElse(0))(p)

  private def might = (p: Player, s: Status) =>
    doSmt(PlayerStrengthLens)(i => i + s.amount.getOrElse(0))(p)

  private def dexterity = (p: Player, s: Status) =>
    doSmt(PlayerDexterityLens)(i => i + s.amount.getOrElse(0))(p)

  private def smart = (p: Player, s: Status) =>
    doSmt(PlayerIntellectLens)(i => i + s.amount.getOrElse(0))(p)

  private def doSmt[T](lens: Lens[T, Int])(f: Int => Int) = lens.modify(f)


}