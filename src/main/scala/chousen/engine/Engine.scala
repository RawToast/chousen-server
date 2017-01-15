package chousen.engine

import chousen.character.{BaseCharacter, Spell}

import scala.util.Random

trait ActionCalc {
  def calcDamage(a: BaseCharacter, d: BaseCharacter): Int = {
    // Base stats calc:
    // Atk: 1-6d + (8 + 8) / 2) = (9-14)
    // Def: 8 / 2 = 4
    // Dmg: (9-14) - 4 = 5-10 + 3 = 8-13
    //
    val dice = Dice.roll()
    val atkPwr = dice + ((a.stats.strength + a.stats.dexterity) / 2)

    val defPwr: Int = d.stats.vitality / 2
    atkPwr - defPwr + 3
  }

  def calcMagic(s: Spell, a: BaseCharacter, d: BaseCharacter): Int = {
    // Base stats fireball (3):
    // 3 + (8 - 8) + 0-3d
    // Dmg = 3-6
    // */
    s.baseDamage + (a.stats.intellect - d.stats.intellect) + Dice.roll(sides = 4, min = 0)
  }
}

object Engine extends ActionCalc

object Dice {
  def roll(sides: Int = 6, min: Int = 1): Int = min + Random.nextInt(sides)
}
