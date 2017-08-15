package chousen.game.status

import chousen.Optics._
import chousen.api.data._
import monocle.Lens

class StatusCalculator {
  def calculate(p: Player): Player = {

    if (p.status.isEmpty) p
    else calcStatusEffects(p)
  }

  def calculate(p: Enemy): Enemy = {

    if (p.status.isEmpty) p
    else calcStatusEffects(p)
  }


  private def calcStatusEffects(player: Player): Player = {

    val status: Seq[Status] = player.status
    val stats = player.stats

    val newStats = status.foldLeft(stats)(foldStatus)

    player.copy(stats = newStats)
  }

  private def calcStatusEffects(enemy: Enemy): Enemy = {

    val status: Seq[Status] = enemy.status
    val stats = enemy.stats

    val newStats = status.foldLeft(stats)(foldStatus)

    enemy.copy(stats = newStats)
  }

  private def foldStatus(p: CharStats, s: Status): CharStats = {
    val func: (CharStats, Status) => CharStats = s.effect match {
      case Fast => fast
      case Slow => slow
      case StoneSkin => stoneskin
      case Might => might
      case Dexterity => dexterity
      case Smart => smart
      case Rage =>
        val m = might(_: CharStats, s)
        val d = dexterity(_: CharStats, s)
        val f = fast(_: CharStats, s)

        (p: CharStats, _: Status) => m.andThen(f).andThen(d).apply(p)
      case Block => nop
      case Poison => nop
      case Regen => nop
    }
    func(p, s)
  }


  private def nop = (p: CharStats, _: Status) => p

  private def fast = (p: CharStats, s: Status) =>
    doSmt(SpeedLens)(i => i + s.amount.getOrElse(0))(p)

  private def slow = (p: CharStats, s: Status) =>
    doSmt(SpeedLens)(i => Math.max(1, i - s.amount.getOrElse(0)))(p)

  private def stoneskin = (p: CharStats, s: Status) =>
    doSmt(VitalityLens)(i => i + s.amount.getOrElse(0))(p)

  private def might = (p: CharStats, s: Status) =>
    doSmt(StrengthLens)(i => i + s.amount.getOrElse(0))(p)

  private def dexterity = (p: CharStats, s: Status) =>
    doSmt(DexterityLens)(i => i + s.amount.getOrElse(0))(p)

  private def smart = (p: CharStats, s: Status) =>
    doSmt(IntellectLens)(i => i + s.amount.getOrElse(0))(p)



  private def doSmt[T](lens: Lens[T, Int])(f: Int => Int) = lens.modify(f)


}