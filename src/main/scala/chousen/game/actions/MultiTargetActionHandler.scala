package chousen.game.actions

import java.util.UUID

import chousen.api.data._
import chousen.game.core.GameStateOptics

object MultiTargetActionHandler extends ActionHandler {

  def handle(targetId: Set[UUID], action: MultiAction): (GameState) => GameState = (gs: GameState) => {

    val message = action.toString.head + ('A' to 'Z').foldLeft(action.toString.tail){case (str: String, c: Char) =>
        str.replace(s"$c", s" $c")
    }

    val targetMsg = GameMessage(s"${gs.player.name} uses $message!")
    val gsWithMessage = gs.copy(messages = gs.messages ++ Seq(targetMsg))

    val newState = handleDead(targetId.foldLeft(gsWithMessage) { case (gs: GameState, id: UUID) =>
      GameStateOptics.targettedLens(id).modify {
        case (p, es, msgs) =>
          es match {
            case Some(e) => actions(action)(p, e, msgs)
            case None => (p, es, msgs)
          }
      }.apply(gs)
    })

    if (gsWithMessage == newState) gs
    else GameStateOptics.PlayerLens.composeLens(PlayerOptics.PlayerPositionLens)
      .modify(p => p - 100)
      .andThen(handleDead)
      .apply(newState)
  }

  private def actions(action: MultiAction): (Player, Enemy, Seq[GameMessage]) => (Player, Option[Enemy], Seq[GameMessage]) = {
    action match {
      case Fireball => fireball
    }
  }

  def fireball(p: Player, e: Enemy, msgs: Seq[GameMessage]) = {
    val dmg = Math.max(1, p.stats.intellect)
    val dmgMsg = msgs :+ GameMessage(s"${e.name} takes $dmg damage.")

    // This should be replaced by a generic attack/damage function
    val newE = EnemyOptics.charStats.composeLens(CharStatsOptics.hp)
      .modify(hp => hp - dmg)(e)

    val gameMessages: Seq[GameMessage] = msgs ++ dmgMsg

    (p, Option(newE), gameMessages)
  }
}
