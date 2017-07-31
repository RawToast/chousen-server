package chousen.game.actions

import chousen.api.data._
import chousen.game.status.StatusCalculator

class DamageCalculator(val sc: StatusCalculator) {

  def calculatePlayerDamage(p: Player, e: Enemy, m: Multipliers=Multipliers()): Int = {
    val sePlayer = sc.calculate(p)
    val player = Participant(sePlayer.stats, sePlayer.status, sePlayer.position)
    val enemy =  Participant(e.stats, Seq.empty, e.position)

    calcDamage(player, enemy, m)
  }

  def calculateEnemyDamage(e: Enemy, p: Player, m: Multipliers=Multipliers()): Int = {
    val sePlayer = sc.calculate(p)

    val player = Participant(sePlayer.stats, sePlayer.status, sePlayer.position)
    val enemy =  Participant(e.stats, Seq.empty, e.position)

    calcDamage(enemy, player, m)
  }

  private case class Participant(stats: CharStats, status: Seq[Status], position: Int)

  private def calcDamage(attacker: Participant, defender: Participant, m: Multipliers): Int = {
    val atkStatusEffects = attacker.status.map(_.effect)
    val mightDamage = if (atkStatusEffects.contains(Might)) defender.stats.maxHp / 10 else 0

    val atkStr = m.str(attacker.stats.strength)
    val atkDex = m.dex(attacker.stats.dexterity)

    val min = atkDex / 2
    val max = atkStr + (atkDex / 2) - defender.stats.vitality

    val dmg: Int = Math.max(min, max) + mightDamage

    dmg
  }
}

case class Multipliers(str: Int => Int= i => i, dex: Int => Int= i => i)

object Multipliers {
  case class Builder(m: Multipliers) {
    def dexMulti(i: Int => Int) = Builder(m.copy(dex = i))
    def strMulti(i: Int => Int) = Builder(m.copy(str = i))
  }
  def builder = Builder(Multipliers())

  val lowMulti = (i:Int) => {i * 1.5}.toInt
  val medMulti = (i:Int) => i * 2
  val highMulti = (i:Int) => {i * 2.5}.toInt

  val lowStrengthSkill = Multipliers(str = lowMulti)
  val strengthSkill = Multipliers(str = medMulti)
  val highStrengthSkill = Multipliers(str = highMulti)


  val dexteritySkill = Multipliers(dex = medMulti)


}
