package chousen.game.status

import chousen.Optics.{MessagesLens, PlayerHealthLens, PlayerLens, PlayerStatusLens}
import chousen.api.data._
import chousen.util.LensUtil


class PostTurnStatusCalculator extends PostTurnStatusCalc

trait PostTurnStatusCalc {

  def applyStatusEffects(game: GameState): GameState = {
    // Apply any Player effecting status effects:
    LensUtil.duoLens(PlayerLens, MessagesLens)
      .modify(playerStatusEffects)
      .andThen(reducePlayerStatuses)(game)
  }


  private def playerStatusEffects(pm: (Player, Seq[GameMessage])): (Player, Seq[GameMessage]) = playerStatusEffects(pm._1, pm._2)
  private def playerStatusEffects(p: Player, msgs: Seq[GameMessage]) = {
    import cats.instances.option.catsKernelStdMonoidForOption
    import cats.instances.int.catsKernelStdGroupForInt
    import cats.implicits.catsSyntaxSemigroup


    val regenEffects = p.status
      .map(s => if (s.effect == Tree) s.copy(effect = Regen) else s)
      .filter(_.effect == Regen)
      .reduceLeftOption[Status] { case (a, b) => a.copy(amount = a.amount |+| b.amount) }


    val effectsForComputation: Seq[Status] = p.status.filterNot(_.effect == Regen) ++ regenEffects

    effectsForComputation.foldLeft(p -> msgs) {
      case (pm: (Player, Seq[GameMessage]), st: Status) =>
        val p = pm._1
        val m = pm._2

        st.effect match {
          case Regen =>
            val regenAmt = st.amount.getOrElse(0)
            val regendPlayer = PlayerHealthLens.modify(hp => Math.min(p.stats.maxHp, hp + regenAmt))(p)
            val heal = regendPlayer.stats.currentHp - p.stats.currentHp
            if (heal > 0) regendPlayer -> (m :+ GameMessage(s"${p.name} regenerates $heal health"))
            else regendPlayer -> m
          case _ => pm
        }
    }
  }

  private def reducePlayerStatuses(gs: GameState) =
    if(gs.player.position >= 150) gs
    else PlayerLens.composeLens(PlayerStatusLens).modify(reduceStatusLength)(gs)

  def reduceStatusLength(status: Seq[Status]): Seq[Status] =
    status.filter(_.turns != 0).map(s => s.copy(turns = s.turns - 1))
}
