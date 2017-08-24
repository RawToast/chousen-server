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

      case QuickAttack => quickAttack
      case Assassinate => assassinate

      case Pain => pain
      case MagicMissile => magicMissile
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
    val sePlayer = damageCalculator.sc.calculate(p)

    val newEnemy = LensUtil.duoLens(EnemyStatsLens.composeLens(HpLens), EnemyStatsLens.composeLens(VitalityLens))
        .modify{case (hp, vit) => hp - dmg -> Math.max(1, vit - (sePlayer.stats.strength / 2)) }
          .apply(e)

    val vitLoss = e.stats.vitality - newEnemy.stats.vitality

    val gameMessages = if (vitLoss > 0) {
      val targetMsg = GameMessage(s"${p.name} lands a destructive blow on ${e.name}!")

      val dmgMsg = GameMessage(s"${e.name}'s loses $vitLoss Vitality and takes $dmg damage.")
      msgs :+ targetMsg :+ dmgMsg
    } else msgs :+ GameMessage(s"${p.name} lands a destructive blow on ${e.name} and deals $dmg damage!")

    (p.copy(position = p.position - 110 + (p.stats.strength / 4)), Option(newEnemy), gameMessages)
  }


  // Dex
  def quickAttack(p: Player, e: Enemy, msgs: Seq[GameMessage]) = {
    val dmg = damageCalculator.calculatePlayerDamage(p, e, Multipliers.dexteritySkill)

    val targetMsg = GameMessage(s"${p.name} uses Quick Attack!")
    val dmgMsg = GameMessage(s"${e.name} takes $dmg damage.")

    val newEnemy = EnemyStatsLens.composeLens(HpLens)
      .modify(hp => hp - dmg)(e)
    val gameMessages = msgs :+ targetMsg :+ dmgMsg

    (p.copy(position = p.position - 80 + (p.stats.dexterity / 2)), Option(newEnemy), gameMessages)
  }

  def assassinate(p: Player, e: Enemy, msgs: Seq[GameMessage]): (Player, Option[Enemy], Seq[GameMessage]) = {

    val sePlayer = damageCalculator.sc.calculate(p)
    val seEnemy = damageCalculator.sc.calculate(e)

    val hpDiff = e.stats.maxHp - e.stats.currentHp
    val reduce = Math.min(1d, sePlayer.stats.dexterity.toDouble / seEnemy.stats.vitality.toDouble)

    val dmg = Math.max(sePlayer.stats.dexterity / 4d, hpDiff * reduce).toInt

    val targetMsg = GameMessage(s"${p.name} uses Assassinate!")
    val dmgMsg = GameMessage(s"${e.name} takes $dmg damage.")

    val newEnemy = EnemyStatsLens.composeLens(HpLens)
      .modify(hp => hp - dmg)(e)
    val gameMessages = msgs :+ targetMsg :+ dmgMsg

    (p.copy(position = p.position - 100), Option(newEnemy), gameMessages)
  }


  // Int
  def pain(p: Player, e: Enemy, msgs: Seq[GameMessage]) = {

    val sePlayer = damageCalculator.sc.calculate(p)
    val seEnemy = damageCalculator.sc.calculate(e)

    val dmg = Math.max(p.stats.intellect / 2d, e.stats.currentHp * Math.min(0.5, sePlayer.stats.intellect.toDouble / seEnemy.stats.vitality.toDouble)).toInt

    val targetMsg = GameMessage(s"${p.name} uses Pain!")
    val dmgMsg = GameMessage(s"${e.name} convulses in pain and takes $dmg damage!")

    val newEnemy = EnemyStatsLens.composeLens(HpLens)
      .modify(hp => hp - dmg)(e)
    val gameMessages = msgs :+ targetMsg :+ dmgMsg

    (p.copy(position = p.position - 100), Option(newEnemy), gameMessages)
  }

  def magicMissile(p: Player, e: Enemy, msgs: Seq[GameMessage]) = {
    val magicDmg = damageCalculator.calculatePlayerMagicDamage(p, e, Multipliers.intellectSkill)

    val dmg = Math.max(1, 2 + magicDmg)

    val targetMsg = GameMessage(s"${p.name} uses Magic Missile!")
    val dmgMsg = GameMessage(s"The missile strikes ${e.name} for $dmg damage!")

    // This should be replaced by a generic attack/damage function
    val newEnemy = EnemyStatsLens.composeLens(HpLens)
      .modify(hp => hp - dmg)(e)
    val gameMessages = msgs :+ targetMsg :+ dmgMsg

    (p.copy(position = p.position - 100), Option(newEnemy), gameMessages)
  }
}
