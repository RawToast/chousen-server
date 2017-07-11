package chousen.game.actions

import java.util.UUID

import chousen.api.data.{CharStatsOptics, EnemyOptics, GameMessage}
import chousen.game.core.GameStateOptics

object BasicAttack extends ActionHandler{

  def attack(targetId: UUID) = GameStateOptics.targettedLens(targetId).modify {
    case (p, optE, msgs) =>

      optE match {
        case Some(e) =>
          val dmg = Math.max(1, p.stats.strength + p.stats.dexterity - e.stats.vitality)

          val targetMsg = GameMessage(s"${p.name} attacks ${e.name}.")
          val dmgMsg = GameMessage(s"${p.name}'s attack deals $dmg to ${e.name}!")

          // This should be replaced by a generic attack/damage function
          val newEnemy = EnemyOptics.EnemyStats.composeLens(CharStatsOptics.HpLens)
            .modify(hp => hp - dmg)(e)
          val gameMessages = msgs :+ targetMsg :+ dmgMsg

          (p.copy(position = p.position - 100), Option(newEnemy), gameMessages)
        case None => (p, optE, msgs)
      }
  }.andThen(handleDead)
}