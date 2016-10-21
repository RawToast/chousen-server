package chousen.engine

import chousen.character.{BaseCharacter, Spell}

import scala.util.Random

object Engine {
  def calcDamage(a: BaseCharacter, d: BaseCharacter): Int = {
    /** Base stats calc:
      * Atk: 1-6d + (4 + 4) / 2) = 5 - 10
      * Def: 8 / 2 = 4
      * Dmg: 1-6 + 3 = 4-9
      */
    val atkPwr = Dice.roll() + ((a.strength + a.dexterity) / 2)
    val defPwr: Int = d.vitality / 2
    atkPwr - defPwr + 3
  }

  def calcMagic(s: Spell, a: BaseCharacter, d: BaseCharacter): Int = {
    /** Base stats fireball (3):
      * 3 + (8 - 8) + 0-3d
      * Dmg = 3-6
      */
    s.baseDamage + (a.intellect - d.intellect) + Dice.roll(sides = 4, min = 0)
  }
}

object Dice {
  def roll(sides: Int = 6, min: Int = 1): Int = min + Random.nextInt(sides - 1)
}