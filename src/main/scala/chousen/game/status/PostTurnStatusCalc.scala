package chousen.game.status

import chousen.Optics.{MessagesLens, PlayerHealthLens, PlayerLens, PlayerStatusLens}
import chousen.api.data._
import chousen.util.LensUtil


class PostTurnStatusCalculator extends PostTurnStatusCalc

trait PostTurnStatusCalc {

    def applyStatusEffects(game: GameState): GameState = {

        // Apply any Player effecting status effects:
        LensUtil.duoLens(PlayerLens, MessagesLens).modify { case (p: Player, msgs: Seq[GameMessage]) =>

            p.status.foldLeft(p -> msgs){
                case (pm: (Player, Seq[GameMessage]), st: Status) =>
                val p = pm._1
                val m = pm._2

                st.effect match {
                    case Regen =>
                    val regenAmt = st.amount.getOrElse(0)
                    val regendPlayer = PlayerHealthLens.modify(hp => Math.min(p.stats.maxHp, hp + regenAmt))(p)
                    val heal = regendPlayer.stats.currentHp - p.stats.currentHp

                    regendPlayer -> (m :+ GameMessage(s"${p.name} regenerates $heal health"))
                    case _ => pm
                }
            }
        }.andThen(PlayerLens.composeLens(PlayerStatusLens).modify(reduceStatusLength))(game)
    }


    def reduceStatusLength(status: Seq[Status]): Seq[Status] =
    status.filter(_.turns != 0).map(s => s.copy(turns = s.turns - 1))
}
