package chousen.game.actions

import java.util.UUID

import chousen.Optics._
import chousen.api.data.{GameMessage, GameState}

class BasicAttack(damageCalculator: DamageCalculator) extends ActionHandler{

  def attack(targetId: UUID): (GameState) => GameState = targettedLens(targetId).modify {
    case (p, optE, msgs) =>

      optE match {
        case Some(e) =>

          val dmg = damageCalculator.calculatePlayerDamage(p, e)

          val targetMsg = GameMessage(s"${p.name} attacks ${e.name}.")
          val dmgMsg = GameMessage(s"${p.name}'s attack deals $dmg to ${e.name}!")

          // This should be replaced by a generic attack/damage function
          val newEnemy = EnemyHpLens.modify(hp => hp - dmg)(e)
          val gameMessages = msgs :+ targetMsg :+ dmgMsg

          (p.copy(position = p.position - 100), Option(newEnemy), gameMessages)
        case None => (p, optE, msgs)
      }
  }.andThen(handleDead)
}