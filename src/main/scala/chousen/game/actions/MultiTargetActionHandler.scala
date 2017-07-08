package chousen.game.actions

import java.util.UUID

import chousen.Optics.{EnemyStats, HpLens}
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
      case MassDrain => massDrain
    }
  }

  def fireball(p: Player, e: Enemy, msgs: Seq[GameMessage]) = {
    val dmg = Math.max(1, (p.stats.intellect * 3) / 2)
    val gameMessages = msgs :+ GameMessage(s"${e.name} is engulfed in flames and takes $dmg damage.")

    // This should be replaced by a generic attack/damage function
    val newE = EnemyOptics.EnemyStats.composeLens(CharStatsOptics.HpLens)
      .modify(hp => hp - dmg)(e)

    (p, Option(newE), gameMessages)
  }

  def staticField(p: Player, e: Enemy, msgs: Seq[GameMessage]) = {
    // Calc isn't accurate
    val dmg = Math.max(1, e.stats.currentHp / 3)
    val gameMessages = msgs :+ GameMessage(s"${e.name} is shocked and takes $dmg damage.")

    // This should be replaced by a generic attack/damage function
    val newE = EnemyOptics.EnemyStats.composeLens(CharStatsOptics.HpLens)
      .modify(hp => hp - dmg)(e)

    (p, Option(newE), gameMessages)
  }

  def shatter(p: Player, e: Enemy, msgs: Seq[GameMessage]) = {
    val dmg = Math.max(p.stats.intellect, p.stats.intellect + p.stats.currentHp - e.stats.vitality)

    val gameMessages = msgs :+ GameMessage(s"The world shakes around ${e.name} dealing $dmg damage!")

    // This should be replaced by a generic attack/damage function
    val newE = EnemyOptics.EnemyStats.composeLens(CharStatsOptics.HpLens)
      .modify(hp => hp - dmg)(e)

    (p, Option(newE), gameMessages)
  }

  def groundStrike(p: Player, e: Enemy, msgs: Seq[GameMessage]) = {
    val dmg = Math.max(1, 4 + p.stats.strength - e.stats.vitality)
    val gameMessages = msgs :+ GameMessage(s"${e.name} takes $dmg damage.")

    // This should be replaced by a generic attack/damage function
    val newE = EnemyOptics.EnemyStats.composeLens(CharStatsOptics.HpLens).modify(hp => hp - dmg)
      .andThen(EnemyOptics.EnemyPosition.modify(ep => ep - 10 - (p.stats.strength / 2)))
        .apply(e)

    (p, Option(newE), gameMessages)
  }

  def windStrike(p: Player, e: Enemy, msgs: Seq[GameMessage]) = {
    val dmg = Math.max(1, (p.stats.dexterity * 2) + (p.stats.intellect / 2) - e.stats.vitality)
    val gameMessages = msgs :+ GameMessage(s"${e.name} takes $dmg damage.")

    // This should be replaced by a generic attack/damage function
    val newE = EnemyOptics.EnemyStats.composeLens(CharStatsOptics.HpLens)
      .modify(hp => hp - dmg)(e)

    (p, Option(newE), gameMessages)
  }

  def massDrain(p: Player, e: Enemy, msgs: Seq[GameMessage]) = {
    val dmg = Math.max(1, p.stats.intellect + (e.stats.maxHp / 12))

    val dmgMsg = GameMessage(s"${p.name} drains $dmg health from ${e.name}!")

    // This should be replaced by a generic attack/damage function
    val newEnemy = EnemyStats.composeLens(HpLens)
      .modify(hp => hp - dmg)(e)

    val newPlayer = PlayerOptics.PlayerHealthLens.modify(hp => Math.min(hp + dmg, p.stats.maxHp))(p)
    val gameMessages = msgs :+ dmgMsg :+ dmgMsg

    (newPlayer, Option(newEnemy), gameMessages)
  }
}
