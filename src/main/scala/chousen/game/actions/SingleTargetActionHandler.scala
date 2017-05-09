package chousen.game.actions

import java.util.UUID

import chousen.api.data._
import chousen.game.core.GameStateOptics

object SingleTargetActionHandler {

  def handle(targetId: UUID, actionId: ActionId) = {

    GameStateOptics.targettedLens(targetId).modify {
      case (p, es, msgs) =>
       actionId match {
        case CrushingBlow => crushingBlow(p, es, msgs)
        case QuickAttack => quickAttack(p, es, msgs)
      }
    }.andThen(handleDead)
  }



  def crushingBlow(p: Player, optE: Option[Enemy], msgs: Seq[GameMessage]) = {
    optE match {
      case Some(e) =>
        // Not safe, could deal negative damage!
        val dmg = (p.stats.strength * 2) + p.stats.dexterity - e.stats.vitality

        val targetMsg = GameMessage(s"${p.name} jumps in the air and lands a crushing blow to ${e.name}!")
        val dmgMsg = GameMessage(s"${e.name} takes $dmg.")

        // This should be replaced by a generic attack/damage function
        val newEnemy = EnemyOptics.charStats.composeLens(CharStatsOptics.hp)
          .modify(hp => hp - dmg)(e)
        val gameMessages = msgs :+ targetMsg :+ dmgMsg

        (p.copy(position = p.position - 130), Option(newEnemy), gameMessages)
      case None => (p, optE, msgs)
    }
  }

  def quickAttack(p: Player, optE: Option[Enemy], msgs: Seq[GameMessage]) = {
    optE match {
      case Some(e) =>
        // Not safe, could deal negative damage!
        val dmg = (p.stats.dexterity * 2) - e.stats.vitality - 2

        val targetMsg = GameMessage(s"${p.name} uses Quick Attack!")
        val dmgMsg = GameMessage(s"${e.name} takes $dmg.")

        // This should be replaced by a generic attack/damage function
        val newEnemy = EnemyOptics.charStats.composeLens(CharStatsOptics.hp)
          .modify(hp => hp - dmg)(e)
        val gameMessages = msgs :+ targetMsg :+ dmgMsg

        (p.copy(position = p.position - 70), Option(newEnemy), gameMessages)
      case None => (p, optE, msgs)
    }
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