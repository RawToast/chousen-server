package chousen.game.actions

import java.util.UUID

import chousen.api.data.{CharStatsOptics, EnemyOptics, GameMessage}
import chousen.game.core.GameStateOps

object BasicAttack {

  def attack(targetId: UUID) = GameStateOps.targettedLens(targetId).modify {
    case (p, optE, msgs) =>

      optE match {
        case Some(e) =>
          // Not safe, could deal negative damage!
          val dmg = p.stats.strength + p.stats.dexterity - e.stats.vitality

          val targetMsg = GameMessage(s"${p.name} attacks ${e.name}!")
          val dmgMsg = GameMessage(s"${p.name} deals $dmg to ${e.name}!")

          // This should be replaced by a generic attack/damage function
          val newEnemy = EnemyOptics.charStats.composeLens(CharStatsOptics.hp)
            .modify(hp => hp - dmg)(e)
          val gameMessages = msgs :+ targetMsg :+ dmgMsg

          (p.copy(position = p.position - 100), Option(newEnemy), gameMessages)
        case None => (p, optE, msgs)
      }
  }

}
