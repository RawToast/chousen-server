package chousen.game.actions

import chousen.Optics._
import chousen.api.data.{Experience, GameMessage, GameState, Player}
import chousen.util.LensUtil

trait ActionHandler {
  def handleDead: (GameState) => GameState = EncounterLens.modify {
    case (p, es, msgs) =>
      val aliveEnemies = es.filter(_.stats.currentHp > 0)
      val deadEnemies = es.filter(_.stats.currentHp < 0).toSeq

      val battleExp = deadEnemies.size
      val expMsg =battleExp match {
        case 0 => Seq.empty
        case _ => Seq(GameMessage(s"${p.name} gained $battleExp experience."))
      }

      val (pl, lvlupMsgs) = levelUp(PlayerCurrentExperienceLens.modify(_+deadEnemies.size)(p))



      val newMessages = deadEnemies
        .map(e =>
          if (e.stats.currentHp > -10) GameMessage(s"${e.name} dies")
          else GameMessage(s"${e.name} is annihilated!")) ++ lvlupMsgs

      (pl, aliveEnemies, msgs ++: expMsg ++: newMessages)
  }

  def levelUp(player: Player): (Player, Seq[GameMessage]) = {

    val exp = player.experience

    if (exp.current >= exp.next) {
      val remainder = exp.current - exp.next
      val newLevel =  exp.level + 1
      val newNext = exp.next + exp.level

      val newExp = Experience(remainder, newNext, newLevel)

      val lens = LensUtil.duoLens(PlayerMaxHealthLens, PlayerHealthLens)
        .modify { case (maxHp: Int, hp: Int) => {
          val newMax = maxHp + 10
          (newMax, Math.min(newMax, hp + newLevel))
        }}.andThen(SetPlayerStats(1, 1, 1, 1))
            .andThen(PlayerExperienceLens.set(newExp))


      (lens(player), Seq(GameMessage(s"${player.name} leveled up to level $newLevel!")))

    } else {
      (player, Seq.empty)
    }
  }
}
