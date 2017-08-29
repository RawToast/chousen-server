package chousen.game.core.turn

import chousen.api.data.{Player, Rage}

object PositionCalculator {

    val TURTLE = 150
    val SLOW = 130
    val SLUGGISH = 110
    val STANDARD = 100
    val ENHANCED = 90
    val QUICK = 70
    val FAST = 50

    def calculatePosition(player: Player, cost: Int = STANDARD, bonus: Int = 0): Player = {
      val dexBonus = player.stats.dexterity / 2
      val rageBonus = player.status.find(_.effect == Rage).fold(0)(r => Math.min(15, (r.amount.getOrElse(4) - 4) * 2))
      player.copy(position = player.position - cost - dexBonus - bonus - rageBonus)
    }
  }
