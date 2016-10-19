package chousen.engine

import chousen.Stats
import chousen.character.{BaseCharacter, Spell}

import scala.util.Random

object Engine {
  def calcDamage(a: BaseCharacter, d: BaseCharacter): Int = {
    val atkPwr = Dice.roll() + ((a.strength + a.dexterity) / 2)
    val defPwr: Int = d.vitality / 2
    atkPwr - defPwr
  }

  def calcMagic(s: Spell, a: BaseCharacter, d: BaseCharacter): Int = {
    val intPwr = a.intellect - Stats.DEFAULT_INTELLECT

    val atkPwr = s.baseDamage + Dice.roll(
      sides = 2 + a.intellect,
      min = if (intPwr > 2) intPwr - 2 else 0)

    val defPwr = d.intellect - Stats.DEFAULT_INTELLECT

    atkPwr - defPwr
  }
}

object Dice {
  def roll(sides: Int = 6, min: Int = 1): Int = min + Random.nextInt(sides - 1)
}