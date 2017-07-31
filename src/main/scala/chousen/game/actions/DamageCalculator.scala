package chousen.game.actions

import chousen.api.data.{CharStats, Enemy, Player, Status}
import chousen.game.status.StatusCalculator

class DamageCalculator(val sc: StatusCalculator) {

  def calculatePlayerDamage(p: Player, e: Enemy): Int = {
    val sePlayer = sc.calculate(p)
    val player = Participant(sePlayer.stats, sePlayer.status)
    val enemy =  Participant(e.stats, Seq.empty)

    calcDamage(player, enemy)
  }

  def calculateEnemyDamage(e: Enemy, p: Player): Int = {
    val sePlayer = sc.calculate(p)

    val player = Participant(sePlayer.stats, sePlayer.status)
    val enemy =  Participant(e.stats, Seq.empty)

    calcDamage(enemy, player)
  }

  private case class Participant(stats: CharStats, status: Seq[Status])

  private def calcDamage(attacker: Participant, defender: Participant): Int = {

    val min = attacker.stats.dexterity / 2
    val max = attacker.stats.strength + attacker.stats.dexterity - defender.stats.vitality

    val dmg: Int = Math.max(min, max)

    dmg
  }

}
