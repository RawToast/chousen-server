package chousen.game.actions

import java.util.UUID

import chousen.api.data._
import chousen.game.core.GameStateOptics

object SingleTargetActionHandler {

  def handle(targetId: UUID, action: SingleTargetAction): (GameState) => GameState = {
    GameStateOptics.targettedLens(targetId).modify {
      case (p, es, msgs) =>
        es match {
          case Some(e) => singleTargetActions(action)(p, e, msgs)
          case None => (p, es, msgs)
        }
    }.andThen(handleDead)
  }

//
//  private def handleMultiTargetAction(targetId: Set[UUID], action: MultiAction): (GameState) => GameState = {
//    GameStateOptics.targettedLens(targetId.head).modify {
//      case (p, es, msgs) =>
//        es match {
//          case Some(e) => singleTargetActions(action)(p, e, msgs)
//          case None => (p, es, msgs)
//        }
//    }.andThen(handleDead)
//  }

//  private def handleSelfAction(action: SelfAction) = GameStateOptics.PlayerLens.modify _


  private def singleTargetActions(actionId: SingleTargetAction): (Player, Enemy, Seq[GameMessage]) => (Player, Option[Enemy], Seq[GameMessage]) =
    actionId match {
      case CrushingBlow => crushingBlow
      case QuickAttack => quickAttack
    }




  def crushingBlow(p: Player, e: Enemy, msgs: Seq[GameMessage]) = {
        // Not safe, could deal negative damage!
        val dmg = (p.stats.strength * 2) + p.stats.dexterity - e.stats.vitality

        val targetMsg = GameMessage(s"${p.name} jumps in the air and lands a crushing blow to ${e.name}!")
        val dmgMsg = GameMessage(s"${e.name} takes $dmg damage.")

        // This should be replaced by a generic attack/damage function
        val newEnemy = EnemyOptics.charStats.composeLens(CharStatsOptics.hp)
          .modify(hp => hp - dmg)(e)
        val gameMessages = msgs :+ targetMsg :+ dmgMsg

      (p.copy(position = p.position - 130), Option(newEnemy), gameMessages)
  }


  def quickAttack(p: Player, e: Enemy, msgs: Seq[GameMessage]) = {
      // Not safe, could deal negative damage!
      val dmg = (p.stats.dexterity * 2) - e.stats.vitality - 2

      val targetMsg = GameMessage(s"${p.name} uses Quick Attack!")
      val dmgMsg = GameMessage(s"${e.name} takes $dmg damage.")

      // This should be replaced by a generic attack/damage function
      val newEnemy = EnemyOptics.charStats.composeLens(CharStatsOptics.hp)
        .modify(hp => hp - dmg)(e)
      val gameMessages = msgs :+ targetMsg :+ dmgMsg

      (p.copy(position = p.position - 70), Option(newEnemy), gameMessages)
  }


  private def handleDead = GameStateOptics.EncounterLens.modify {
    case (p, es, msgs) =>

      val aliveEnemies = es.filter(_.stats.currentHp > 0)
      val newMessages = es.filter(_.stats.currentHp < 0)
        .map(e => GameMessage(s"${e.name} dies"))
        .toSeq

      (p, aliveEnemies, msgs ++: newMessages)
  }
}