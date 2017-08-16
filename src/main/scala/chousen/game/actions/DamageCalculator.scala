package chousen.game.actions

import chousen.api.data._
import chousen.game.status.StatusCalculator

class DamageCalculator(val sc: StatusCalculator) {

  def calculatePlayerDamage(p: Player, e: Enemy, m: Multipliers = Multipliers()): Int = {
    val sePlayer = sc.calculate(p)
    val seEnemy = sc.calculate(e)

    val player = Participant(sePlayer.stats, sePlayer.status, sePlayer.equipment, sePlayer.position)
    val enemy = Participant(seEnemy.stats, seEnemy.status, Equipment(), seEnemy.position)

    calcDamage(player, enemy, m)
  }

  def calculateEnemyDamage(e: Enemy, p: Player, m: Multipliers = Multipliers()): Int = {
    val sePlayer = sc.calculate(p)
    val seEnemy = sc.calculate(e)

    val player = Participant(sePlayer.stats, sePlayer.status, sePlayer.equipment, sePlayer.position)
    val enemy = Participant(seEnemy.stats, seEnemy.status, Equipment(), seEnemy.position)

    calcDamage(enemy, player, m)
  }

  private case class Participant(stats: CharStats, status: Seq[Status], equipment: Equipment, position: Int)

  private def calcDamage(attacker: Participant, defender: Participant, m: Multipliers): Int = {
    val atkStatusEffects = attacker.status.map(_.effect)
    val defStatusEffects = defender.status.map(_.effect)

    val mightDamage = if (atkStatusEffects.contains(Might)) defender.stats.maxHp / 10 else 0
    val berserkDamage = if (atkStatusEffects.contains(Rage)) attacker.stats.strength / 2 else 0
    val stoneSkin = if (defStatusEffects.contains(StoneSkin)) 3 else 0
    val blockEffect = if (defStatusEffects.contains(Block)) 0.5 else 1

    val atkStr = m.str(attacker.stats.strength)
    val atkDex = m.dex(attacker.stats.dexterity)
    val atkInt = m.int(attacker.stats.intellect)

    val weaponDamage = {
      val weaponDmg = attacker.equipment.weapon.map(_.dmg).getOrElse(0)

      val weaponBonus = attacker.equipment.weapon
        .map(_.effects)
        .getOrElse(Seq.empty)
        .foldLeft(0)((i, we) => i + {
          we match {
            case Magic => atkInt
            case Crush => defender.stats.currentHp / 8
            case Toxic => 0
          }
        })

      weaponDmg + weaponBonus
    }

    val armour = defender.equipment.armour.map(_.defense).getOrElse(stoneSkin)

    implicit class BlockEffect(i: Int) {
      def block: Int = {
        i * blockEffect
      }.toInt
    }

    val min = Math.max(1, (atkDex / 2) - stoneSkin).block
    val max: Int = m.max((atkStr + (atkDex / 2) + mightDamage + berserkDamage + weaponDamage) - defender.stats.vitality - armour).block

    val dmg: Int = Math.max(min, max)

    dmg
  }
}

case class Multipliers(str: Int => Int = i => i,
                       dex: Int => Int = i => i,
                       int: Int => Int = i => i,
                       max: Int => Int = i => i)

object Multipliers {

  case class Builder(m: Multipliers) {
    def dexMulti(i: Int => Int) = Builder(m.copy(dex = i))

    def strMulti(i: Int => Int) = Builder(m.copy(str = i))

    def intMulti(i: Int => Int) = Builder(m.copy(int = i))

    def maxMulti(i: Int => Int) = Builder(m.copy(int = i))
  }

  def builder = Builder(Multipliers())

  val multiTarget = (i: Int) => {
    i * 0.5
  }.toInt

  val lowMulti = (i: Int) => {
    i * 1.5
  }.toInt
  val medMulti = (i: Int) => i * 2
  val highMulti = (i: Int) => {
    i * 2.5
  }.toInt

  val lowStrengthSkill = Multipliers(str = lowMulti)
  val strengthSkill = Multipliers(str = medMulti)
  val highStrengthSkill = Multipliers(str = highMulti)


  val dexteritySkill = Multipliers(dex = medMulti)

  val multiTargetSkill = Multipliers(max = multiTarget)
}
