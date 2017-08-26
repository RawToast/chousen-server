package chousen.game.core.turn

import chousen.Optics._
import chousen.api.data._
import chousen.util.LensUtil

import scala.annotation.tailrec

object PostTurnOps {

  def handleDead(pem: (Player, Set[Enemy], Seq[GameMessage])) = {
    val (p, es, msgs) = pem
      val aliveEnemies = es.filter(_.stats.currentHp > 0)
      val deadEnemies = es.filter(_.stats.currentHp < 0).toSeq

      val battleExp = deadEnemies.map(_.stats.maxHp / 10).sum

      val (pl, lvlupMsgs) = levelUp(PlayerCurrentExperienceLens.modify(xp => xp + battleExp)(p))

      val newMessages = deadEnemies
        .map(e =>
          if (e.stats.currentHp > -10) GameMessage(s"${e.name} dies")
          else GameMessage(s"${e.name} is annihilated!")) ++ lvlupMsgs

      val deNames = deadEnemies.map(_.name)
      val player = if (deNames.contains("")) {


        pl
      } else pl

      (player, aliveEnemies, msgs ++: newMessages)
  }

  def levelUp(player: Player): (Player, Seq[GameMessage]) = {

    val exp = player.experience

    if (exp.current >= exp.next) {

      @tailrec
      def numberOfLevels(e: Experience, acc: Int=0): (Experience, Int) = {
        if (e.current >= e.next) {
          val remainder = e.current - e.next
          val newLevel =  e.level + 1
          val newNext = e.next + (((e.level + 1) * e.level) / 2)

          val newExp = Experience(remainder, newNext, newLevel)

          numberOfLevels(newExp, acc + 1)
        } else e -> acc
      }

      val (newExp, lvls) = numberOfLevels(player.experience)

      val lens = LensUtil.duoLens(PlayerMaxHealthLens, PlayerHealthLens)
        .modify { case (maxHp: Int, hp: Int) => {
          val newMax = maxHp + 10
          (newMax, Math.min(newMax, hp + (10 * lvls)))
        }}.andThen(SetPlayerStats(lvls, lvls, lvls, lvls))
            .andThen(PlayerExperienceLens.set(newExp))


      (lens(player), Seq(GameMessage(s"${player.name} leveled up to level ${newExp.level}!")))

    } else {
      (player, Seq.empty)
    }
  }
}
