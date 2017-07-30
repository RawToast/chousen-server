package chousen.game.actions

import chousen.api.data.{Enemy, Player}

trait DamageCalculator {

  def calculatePlayerDamage(p: Player, e: Enemy): Player

  def calculateEnemyDamage(e: Enemy, p: Player): Player

}
