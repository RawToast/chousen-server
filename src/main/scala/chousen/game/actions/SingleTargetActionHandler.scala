package chousen.game.actions

import java.util.UUID

import chousen.Optics._
import chousen.api.data._
import chousen.game.status.StatusBuilder
import chousen.util.LensUtil

class SingleTargetActionHandler(damageCalculator: DamageCalculator) extends ActionHandler {

  def handle(targetId: UUID, action: SingleTargetAction): (GameState) => GameState = {
    targettedLens(targetId).modify {
      case (p, es, msgs) =>
        es match {
          case Some(e) => singleTargetActions(action)(p, e, msgs)
          case None => (p, es, msgs)
        }
    }.andThen(handleDead)
  }

  private def singleTargetActions(actionId: SingleTargetAction): (Player, Enemy, Seq[GameMessage]) => (Player, Option[Enemy], Seq[GameMessage]) =
    actionId match {
      case CrushingBlow => crushingBlow
      case StunningStrike => stunningStrike
      case Counter => counter
      case Destruction => destruction
    }


  def crushingBlow(p: Player, e: Enemy, msgs: Seq[GameMessage]) = {
    import chousen.game.actions.Multipliers._

        val dmg = damageCalculator.calculatePlayerDamage(p, e,
          builder.strMulti(highMulti).dexMulti(medMulti).m)

        val targetMsg = GameMessage(s"${p.name} jumps in the air and lands a crushing blow to ${e.name}!")
        val dmgMsg = GameMessage(s"${e.name} takes $dmg damage.")

        val newEnemy = EnemyStatsLens.composeLens(HpLens)
          .modify(hp => hp - dmg)(e)
        val gameMessages = msgs :+ targetMsg :+ dmgMsg

      (p.copy(position = p.position - 130 + (p.stats.strength / 4)), Option(newEnemy), gameMessages)
  }

  def stunningStrike(p: Player, e: Enemy, msgs: Seq[GameMessage]) = {
    val dmg = damageCalculator.calculatePlayerDamage(p, e, Multipliers.lowStrengthSkill)
    val sePlayer = damageCalculator.sc.calculate(p)

    val targetMsg = GameMessage(s"${p.name} uses Stunning Strike!")
    val dmgMsg = GameMessage(s"${e.name} is dazed and takes $dmg damage!")

    val newEnemy = EnemyStatsLens.composeLens(HpLens)
        .modify(hp => hp - dmg).
      andThen(EnemyPosition
        .modify(s => s - 70 - sePlayer.stats.strength))
          .andThen(EnemyStatusLens.modify(_ :+ StatusBuilder.makeSlow(2)))(e)
    val gameMessages = msgs :+ targetMsg :+ dmgMsg

    (p.copy(position = p.position - 100), Option(newEnemy), gameMessages)
  }

  def counter(p: Player, e: Enemy, msgs: Seq[GameMessage]) = {
    //TODO use status effected enemy strength
    val dmg = damageCalculator.calculatePlayerDamage(p, e, Multipliers.strengthSkill) + e.stats.strength

    val targetMsg = GameMessage(s"${p.name} uses Counter!")
    val dmgMsg = GameMessage(s"${e.name} is countered and takes $dmg damage.")


    // This should be replaced by a generic attack/damage function
    val newEnemy = EnemyStatsLens.composeLens(HpLens)
      .modify(hp => hp - dmg)(e)
    val gameMessages = msgs :+ targetMsg :+ dmgMsg

    (p.copy(position = p.position - 100), Option(newEnemy), gameMessages)
  }

  def destruction(p: Player, e: Enemy, msgs: Seq[GameMessage]) = {
    val dmg = damageCalculator.calculatePlayerDamage(p, e, Multipliers.lowStrengthSkill)

    val targetMsg = GameMessage(s"${p.name} lands a destructive blow on ${e.name}!")
    val dmgMsg = GameMessage(s"${e.name}'s defense is broken and takes $dmg damage.")

    val newEnemy = LensUtil.duoLens(EnemyStatsLens.composeLens(HpLens), EnemyStatsLens.composeLens(VitalityLens))
        .modify{case (hp, vit) => hp - dmg -> Math.max(1, vit - (p.stats.strength / 2)) }
          .apply(e)

    val gameMessages = msgs :+ targetMsg :+ dmgMsg

    (p.copy(position = p.position - 130 + (p.stats.strength / 4)), Option(newEnemy), gameMessages)
  }
}
