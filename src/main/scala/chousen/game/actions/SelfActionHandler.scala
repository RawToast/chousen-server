package chousen.game.actions

import chousen.api.data._
import chousen.game.core.GameStateOptics.{PlayerLens, MessagesLens}
import chousen.util.LensUtil


object SelfActionHandler {

  def handleSelfAction(action: SelfAction): (GameState) => GameState = {
    LensUtil.duoLens(PlayerLens, MessagesLens).modify{
      case (p:Player, msgs: Seq[GameMessage]) =>
        actions(action)(p, msgs)
    }
  }

  private def actions(actionId: SelfAction): (Player, Seq[GameMessage]) => (Player, Seq[GameMessage]) =
    actionId match {
      case HealWounds => healWounds
    }


  def healWounds(p: Player, msgs: Seq[GameMessage]) = {
    val healAmount = 10 + p.stats.intellect + (p.stats.maxHp / 10)
    val message = GameMessage(s"${p.name} uses Heal Wounds and recovers $healAmount!")
    val gameMessages = msgs :+ message

    val lens = LensUtil.duoLens(PlayerOptics.PlayerHealthLens, PlayerOptics.PlayerPositionLens)
      .modify{case (hp: Int, position: Int) =>
          Math.min(p.stats.maxHp, hp + healAmount) -> (position - 100)
         }
    (lens.apply(p), gameMessages)
  }
}
