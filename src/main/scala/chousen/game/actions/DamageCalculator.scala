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

  def calculatePlayerMagicDamage(p: Player, e: Enemy, m: Multipliers = Multipliers()): Int = {
    val sePlayer = sc.calculate(p)
    val seEnemy = sc.calculate(e)

    val player = Participant(sePlayer.stats, sePlayer.status, sePlayer.equipment, sePlayer.position)
    val enemy = Participant(seEnemy.stats, seEnemy.status, Equipment(), seEnemy.position)

    calcMagicDamage(player, enemy, m)
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
    val berserkDamage = attacker.status.find(_.effect == Rage)
      .fold(0)(s => s.amount.getOrElse(0) + (attacker.stats.strength / 2) - 2)
    val stoneSkin = if (defStatusEffects.contains(StoneSkin)) 4 else 0
    val blockEffect = if (defStatusEffects.contains(Block)) 0.5 else 1
    val treeDef = if (defStatusEffects.contains(Tree)) 10 else 0

    val fortDef: Int = defender.status.filter(_.effect == Fort)
      .map(_.amount)
      .foldLeft(0)((i, o) => i + o.getOrElse(0))

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
            case Magic => atkInt / 2
            case Heavy => atkStr / 2

            case Crush => Math.max(1, defender.stats.currentHp / 8)
            case Deadly => 1 + (defender.stats.maxHp / 12)
            case Maim => Math.max(1, (defender.stats.maxHp - defender.stats.currentHp) / 8)

            case Quick => 1 + (Math.max(0, attacker.stats.speed - 8) * 2)
            case Deceive => Math.max(1, (attacker.stats.maxHp - attacker.stats.currentHp) / 4)

            case Protection => 0
            case Toxic => 0
          }
        })

      weaponDmg + weaponBonus
    }

    val weapBonusArm = defender.equipment.weapon.map(_.effects)
        .getOrElse(Seq.empty)
          .foldLeft(0)((i ,we) => i + {
            we match {
            case Protection => 5
            case _ => 0
          }})

    val armour = defender.equipment.armour.map(_.defense).getOrElse(stoneSkin) + treeDef + weapBonusArm

    implicit class BlockEffect(i: Int) {
      def block: Int = {
        i * blockEffect
      }.toInt
    }

    val bonusDamage = mightDamage + berserkDamage + weaponDamage
    val bonusArmour = armour + stoneSkin + fortDef

    val min = Math.max(1, (atkDex / 2) + (bonusDamage / 4) - (bonusArmour / 4) - stoneSkin).block
    val max: Int = m.max((atkStr + (atkDex / 2) + bonusDamage) - defender.stats.vitality - bonusArmour).block

    val dmg: Int = Math.max(min, max)

    dmg
  }


  private def calcMagicDamage(attacker: Participant, defender: Participant, m: Multipliers): Int = {
    val atkStatusEffects = attacker.status.map(_.effect)
    val defStatusEffects = defender.status.map(_.effect)

    val atkInt = m.int(attacker.stats.intellect)

    val smartDamage = if (atkStatusEffects.contains(Smart)) 4 + (atkInt / 4) else 0
    val stoneSkin = if (defStatusEffects.contains(StoneSkin)) 4 else 0
    val blockEffect = if (defStatusEffects.contains(Block)) 0.5 else 1

    val armour = defender.equipment.armour.map(_.defense).getOrElse(stoneSkin)


    implicit class BlockEffect(i: Int) {
      def block: Int = {
        i * blockEffect
      }.toInt
    }

    val bonusDamage = smartDamage
    val bonusArmour = armour + stoneSkin

    val min = Math.max(1, (atkInt + bonusDamage) - (bonusArmour / 4) - stoneSkin).block
    val max: Int = m.max((atkInt + bonusDamage) - (bonusArmour / 4) - stoneSkin).block

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
  val maxMulti = (i: Int) => {
    i * 3
  }.toInt

  val lowStrengthSkill = Multipliers(str = lowMulti)
  val strengthSkill = Multipliers(str = medMulti)
  val highStrengthSkill = Multipliers(str = highMulti)

  val lowDexteritySkill = Multipliers(str = lowMulti)
  val dexteritySkill = Multipliers(dex = medMulti)

  val lowIntellectSkill = Multipliers(int = lowMulti)
  val intellectSkill = Multipliers(int = medMulti)
//  val highIntellectSkill = Multipliers(int = highMulti)

  val multiTargetSkill = Multipliers(max = multiTarget)
}
