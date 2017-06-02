package chousen.game.actions

import java.util.UUID

import chousen.api.data._
import chousen.game.core.GameStateOptics._

object MultiTargetActionHandler extends ActionHandler {

  def handle(targetId: Set[UUID], action: MultiAction): (GameState) => GameState = (gs: GameState) => {

    val message = action.toString.head + ('A' to 'Z').foldLeft(action.toString.tail){case (str: String, c: Char) =>
        str.replace(s"$c", s" $c")
    }

    val targetMsg = GameMessage(s"${gs.player.name} uses $message!")
    val gsWithMessage = gs.copy(messages = gs.messages ++ Seq(targetMsg))

    val newState = handleDead(targetId.foldLeft(gsWithMessage) { case (gs: GameState, id: UUID) =>
      targettedLens(id).modify {
        case (p, es, msgs) =>
          es match {
            case Some(e) => actions(action)(p, e, msgs)
            case None => (p, es, msgs)
          }
      }.apply(gs)
    })

    if (gsWithMessage == newState) gs
    else {

      val finalState = if(action == Shatter)  {
        PlayerLens.composeLens(PlayerOptics.PlayerHealthLens).set(1)
          .andThen(MessagesLens.modify(msgs =>
            msgs :+ GameMessage(s"${newState.player.name} is caught in the chaos and is left with 1 health!")))
            .apply(newState)} else newState

      PlayerLens.composeLens(PlayerOptics.PlayerPositionLens)
      .modify(p => p - 100)
      .andThen(handleDead)
      .apply(finalState)
    }
  }

  private def actions(action: MultiAction): (Player, Enemy, Seq[GameMessage]) => (Player, Option[Enemy], Seq[GameMessage]) = {
    action match {
      case Fireball => fireball
      case StaticField => staticField
      case Shatter => shatter
      case GroundStrike => groundStrike
      case WindStrike => windStrike
    }
  }

  def fireball(p: Player, e: Enemy, msgs: Seq[GameMessage]) = {
    val dmg = Math.max(1, (p.stats.intellect * 3) / 2)
    val gameMessages = msgs :+ GameMessage(s"${e.name} is engulfed in flames and takes $dmg damage.")

    // This should be replaced by a generic attack/damage function
    val newE = EnemyOptics.charStats.composeLens(CharStatsOptics.hp)
      .modify(hp => hp - dmg)(e)

    (p, Option(newE), gameMessages)
  }

  def staticField(p: Player, e: Enemy, msgs: Seq[GameMessage]) = {
    // Calc isn't accurate
    val dmg = Math.max(1, e.stats.currentHp / 4)
    val gameMessages = msgs :+ GameMessage(s"${e.name} is shocked and takes $dmg damage.")

    // This should be replaced by a generic attack/damage function
    val newE = EnemyOptics.charStats.composeLens(CharStatsOptics.hp)
      .modify(hp => hp - dmg)(e)

    (p, Option(newE), gameMessages)
  }

  def shatter(p: Player, e: Enemy, msgs: Seq[GameMessage]) = {
    val dmg = Math.max(p.stats.intellect, p.stats.intellect + p.stats.currentHp - e.stats.vitality)

    val gameMessages = msgs :+ GameMessage(s"The world shakes around ${e.name} dealing $dmg damage!")

    // This should be replaced by a generic attack/damage function
    val newE = EnemyOptics.charStats.composeLens(CharStatsOptics.hp)
      .modify(hp => hp - dmg)(e)

    (p, Option(newE), gameMessages)
  }

  def groundStrike(p: Player, e: Enemy, msgs: Seq[GameMessage]) = {
    val dmg = Math.max(1, 2 + p.stats.strength - e.stats.vitality)
    val gameMessages = msgs :+ GameMessage(s"${e.name} takes $dmg damage.")

    // This should be replaced by a generic attack/damage function
    val newE = EnemyOptics.charStats.composeLens(CharStatsOptics.hp).modify(hp => hp - dmg)
      .andThen(EnemyOptics.position.modify(p => p - 15))
        .apply(e)

    (p, Option(newE), gameMessages)
  }

  def windStrike(p: Player, e: Enemy, msgs: Seq[GameMessage]) = {
    val dmg = Math.max(1, (p.stats.dexterity * 2) + (p.stats.intellect / 2) - e.stats.vitality)
    val gameMessages = msgs :+ GameMessage(s"${e.name} takes $dmg damage.")

    // This should be replaced by a generic attack/damage function
    val newE = EnemyOptics.charStats.composeLens(CharStatsOptics.hp)
      .modify(hp => hp - dmg)(e)

    (p, Option(newE), gameMessages)
  }
}
