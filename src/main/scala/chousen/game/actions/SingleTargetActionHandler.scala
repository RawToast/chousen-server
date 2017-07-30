package chousen.game.actions

import java.util.UUID

import chousen.Optics._
import chousen.api.data._
import chousen.game.status.StatusCalculator
import chousen.util.LensUtil

class SingleTargetActionHandler(sc: StatusCalculator) extends ActionHandler {

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
      case Hamstring => hamstring
      case StunningStrike => stunningStrike
      case Counter => counter
      case Destruction => destruction

      case QuickAttack => quickAttack
      case Assassinate => assassinate
      case TripleStrike => tripleStrike

      case Pain => pain
      case MagicMissile => magicMissile
      case Drain => drain
    }


  def crushingBlow(p: Player, e: Enemy, msgs: Seq[GameMessage]) = {
        val sePlayer = sc.calculate(p)

        val dmg = Math.max(1, (sePlayer.stats.strength * 2) + sePlayer.stats.dexterity - e.stats.vitality)

        val targetMsg = GameMessage(s"${p.name} jumps in the air and lands a crushing blow to ${e.name}!")
        val dmgMsg = GameMessage(s"${e.name} takes $dmg damage.")

        // This should be replaced by a generic attack/damage function
        val newEnemy = EnemyStats.composeLens(HpLens)
          .modify(hp => hp - dmg)(e)
        val gameMessages = msgs :+ targetMsg :+ dmgMsg

      (p.copy(position = p.position - 130 + (p.stats.strength / 4)), Option(newEnemy), gameMessages)
  }


  def quickAttack(p: Player, e: Enemy, msgs: Seq[GameMessage]) = {
      val sePlayer = sc.calculate(p)

      val dmg = Math.max(1, (sePlayer.stats.dexterity * 2) - e.stats.vitality)

      val targetMsg = GameMessage(s"${p.name} uses Quick Attack!")
      val dmgMsg = GameMessage(s"${e.name} takes $dmg damage.")

      // This should be replaced by a generic attack/damage function
      val newEnemy = EnemyStats.composeLens(HpLens)
        .modify(hp => hp - dmg)(e)
      val gameMessages = msgs :+ targetMsg :+ dmgMsg

      (p.copy(position = p.position - 80 + (p.stats.dexterity / 4)), Option(newEnemy), gameMessages)
  }

  def tripleStrike(p: Player, e: Enemy, msgs: Seq[GameMessage]) = {
    val sePlayer = sc.calculate(p)

    val dmg = Math.max(3, (1 + ((sePlayer.stats.dexterity * 3) / 2) - e.stats.vitality) * 3)

    val targetMsg = GameMessage(s"${p.name} uses Triple Strike!")
    val dmgMsg = GameMessage(s"${e.name} takes $dmg damage.")

    // This should be replaced by a generic attack/damage function
    val newEnemy = EnemyStats.composeLens(HpLens)
      .modify(hp => hp - dmg)(e)
    val gameMessages = msgs :+ targetMsg :+ dmgMsg

    (p.copy(position = p.position - 100), Option(newEnemy), gameMessages)
  }

  def hamstring(p: Player, e: Enemy, msgs: Seq[GameMessage]) = {
    val sePlayer = sc.calculate(p)

    val dmg = Math.max(1, (2 * sePlayer.stats.strength) - (e.stats.vitality / 2) - 4)

    val targetMsg = GameMessage(s"${p.name} uses Hamstring!")
    val dmgMsg = GameMessage(s"${e.name} slows down and takes $dmg damage.")

    // This should be replaced by a generic attack/damage function
    val newEnemy = EnemyOptics.EnemyStats.composeLens(CharStatsOptics.HpLens)
        .modify(hp => hp - dmg).
      andThen(EnemyOptics.EnemyStats.composeLens(CharStatsOptics.SpeedLens)
        .modify(s => Math.min(1, s - 1)))(e)

    val gameMessages = msgs :+ targetMsg :+ dmgMsg

    (p.copy(position = p.position - 100), Option(newEnemy), gameMessages)
  }

  def stunningStrike(p: Player, e: Enemy, msgs: Seq[GameMessage]) = {
    val sePlayer = sc.calculate(p)

    val dmg = Math.max(1, (2 * sePlayer.stats.strength) - e.stats.vitality - 2)

    val targetMsg = GameMessage(s"${p.name} uses Stunning Strike!")
    val dmgMsg = GameMessage(s"${e.name} is stunned and takes $dmg damage!")

    // This should be replaced by a generic attack/damage function
    val newEnemy = EnemyStats.composeLens(HpLens)
        .modify(hp => hp - dmg).
      andThen(EnemyPosition
        .modify(s => s - 100 - p.stats.strength))(e)
    val gameMessages = msgs :+ targetMsg :+ dmgMsg

    (p.copy(position = p.position - 105 + p.stats.strength), Option(newEnemy), gameMessages)
  }

  def counter(p: Player, e: Enemy, msgs: Seq[GameMessage]) = {
    val sePlayer = sc.calculate(p)

    val dmg = Math.max(1, (sePlayer.stats.strength * 2) + e.stats.strength - e.stats.vitality)

    val targetMsg = GameMessage(s"${p.name} uses Counter!")
    val dmgMsg = GameMessage(s"${e.name} is countered and takes $dmg damage.")

    // This should be replaced by a generic attack/damage function
    val newEnemy = EnemyStats.composeLens(HpLens)
      .modify(hp => hp - dmg)(e)
    val gameMessages = msgs :+ targetMsg :+ dmgMsg

    (p.copy(position = p.position - 100), Option(newEnemy), gameMessages)
  }

  def destruction(p: Player, e: Enemy, msgs: Seq[GameMessage]) = {
    val sePlayer = sc.calculate(p)

    val dmg = Math.max(1, (sePlayer.stats.strength * 3) / 2)

    val targetMsg = GameMessage(s"${p.name} lands a destructive blow on ${e.name}!")
    val dmgMsg = GameMessage(s"${e.name}'s defense is broken and takes $dmg damage.")

    // This should be replaced by a generic attack/damage function
    val newEnemy = LensUtil.duoLens(EnemyStats.composeLens(HpLens), EnemyStats.composeLens(VitalityLens))
        .modify{case (hp, vit) => hp - dmg -> Math.max(1, vit - (p.stats.strength / 2)) }
          .apply(e)

    val gameMessages = msgs :+ targetMsg :+ dmgMsg

    (p.copy(position = p.position - 130 + (p.stats.strength / 4)), Option(newEnemy), gameMessages)
  }


  // Dex

  def assassinate(p: Player, e: Enemy, msgs: Seq[GameMessage]): (Player, Option[Enemy], Seq[GameMessage]) = {
    val sePlayer = sc.calculate(p)

    val dmg = Math.max(sePlayer.stats.dexterity, e.stats.maxHp - e.stats.currentHp)

    val targetMsg = GameMessage(s"${p.name} uses Assassinate!")
    val dmgMsg = GameMessage(s"${e.name} takes $dmg damage.")

    // This should be replaced by a generic attack/damage function
    val newEnemy = EnemyStats.composeLens(HpLens)
      .modify(hp => hp - dmg)(e)
    val gameMessages = msgs :+ targetMsg :+ dmgMsg

    (p.copy(position = p.position - 100), Option(newEnemy), gameMessages)
  }

  def pain(p: Player, e: Enemy, msgs: Seq[GameMessage]) = {
    val sePlayer = sc.calculate(p)

    val dmg = Math.max(sePlayer.stats.intellect / 2, e.stats.currentHp / 2)

    val targetMsg = GameMessage(s"${p.name} uses Pain!")
    val dmgMsg = GameMessage(s"${e.name} convulses in pain and takes $dmg damage!")

    // This should be replaced by a generic attack/damage function
    val newEnemy = EnemyStats.composeLens(HpLens)
      .modify(hp => hp - dmg)(e)
    val gameMessages = msgs :+ targetMsg :+ dmgMsg

    (p.copy(position = p.position - 100), Option(newEnemy), gameMessages)
  }

  def magicMissile(p: Player, e: Enemy, msgs: Seq[GameMessage]) = {
    val sePlayer = sc.calculate(p)

    val dmg = Math.max(1, 2 + sePlayer.stats.intellect * 2)

    val targetMsg = GameMessage(s"${p.name} uses Magic Missile!")
    val dmgMsg = GameMessage(s"The missile strikes ${e.name} for $dmg damage!")

    // This should be replaced by a generic attack/damage function
    val newEnemy = EnemyStats.composeLens(HpLens)
      .modify(hp => hp - dmg)(e)
    val gameMessages = msgs :+ targetMsg :+ dmgMsg

    (p.copy(position = p.position - 100), Option(newEnemy), gameMessages)
  }

  def drain(p: Player, e: Enemy, msgs: Seq[GameMessage]) = {
    val sePlayer = sc.calculate(p)

    val dmg = Math.max(1, 1 + sePlayer.stats.intellect + (e.stats.maxHp / 5))

    val targetMsg = GameMessage(s"${p.name} uses Drain!")
    val dmgMsg = GameMessage(s"${p.name} drains $dmg health from ${e.name}!")

    // This should be replaced by a generic attack/damage function
    val newEnemy = EnemyStats.composeLens(HpLens)
      .modify(hp => hp - dmg)(e)
    val newPlayer = PlayerOptics.PlayerHealthLens.modify(hp => Math.min(hp + dmg, p.stats.maxHp)).apply(p)

    val gameMessages = msgs :+ targetMsg :+ dmgMsg

    (newPlayer.copy(position = p.position - 100), Option(newEnemy), gameMessages)
  }
}
