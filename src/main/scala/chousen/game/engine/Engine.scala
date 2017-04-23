package chousen.game.engine

import scala.util.Random

// Should use a seed
object Dice {
  def roll(sides: Int = 6, min: Int = 1): Int = min + Random.nextInt(sides)
}
