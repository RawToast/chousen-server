package chousen.game.actions

import java.util.UUID

import chousen.api.data._
import chousen.game.core.GameStateOptics

object SingleTargetActionHandler extends ActionHandler {

  def handle(targetId: UUID, action: SingleTargetAction): (GameState) => GameState = {
    GameStateOptics.targettedLens(targetId).modify {
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

      case QuickAttack => quickAttack
      case Assassinate => assassinate
      case TripleStrike => tripleStrike

      case Pain => pain
      case MagicMissile => magicMissile
      case Drain => drain
    }


  def crushingBlow(p: Player, e: Enemy, msgs: Seq[GameMessage]) = {
        val dmg = Math.max(1, (p.stats.strength * 2) + p.stats.dexterity - e.stats.vitality)

        val targetMsg = GameMessage(s"${p.name} jumps in the air and lands a crushing blow to ${e.name}!")
        val dmgMsg = GameMessage(s"${e.name} takes $dmg damage.")

        // This should be replaced by a generic attack/damage function
        val newEnemy = EnemyOptics.charStats.composeLens(CharStatsOptics.hp)
          .modify(hp => hp - dmg)(e)
        val gameMessages = msgs :+ targetMsg :+ dmgMsg

      (p.copy(position = p.position - 130), Option(newEnemy), gameMessages)
  }


  def quickAttack(p: Player, e: Enemy, msgs: Seq[GameMessage]) = {
      val dmg = Math.max(1, (p.stats.dexterity * 2) - e.stats.vitality)

      val targetMsg = GameMessage(s"${p.name} uses Quick Attack!")
      val dmgMsg = GameMessage(s"${e.name} takes $dmg damage.")

      // This should be replaced by a generic attack/damage function
      val newEnemy = EnemyOptics.charStats.composeLens(CharStatsOptics.hp)
        .modify(hp => hp - dmg)(e)
      val gameMessages = msgs :+ targetMsg :+ dmgMsg

      (p.copy(position = p.position - 80 - p.stats.dexterity), Option(newEnemy), gameMessages)
  }

  def tripleStrike(p: Player, e: Enemy, msgs: Seq[GameMessage]) = {
    val dmg = Math.max(3, (1 + p.stats.dexterity - e.stats.vitality) * 3)

    val targetMsg = GameMessage(s"${p.name} uses Triple Strike!")
    val dmgMsg = GameMessage(s"${e.name} takes $dmg damage.")

    // This should be replaced by a generic attack/damage function
    val newEnemy = EnemyOptics.charStats.composeLens(CharStatsOptics.hp)
      .modify(hp => hp - dmg)(e)
    val gameMessages = msgs :+ targetMsg :+ dmgMsg

    (p.copy(position = p.position - 100), Option(newEnemy), gameMessages)
  }

  def hamstring(p: Player, e: Enemy, msgs: Seq[GameMessage]) = {
    val dmg = Math.max(1, (2 * p.stats.strength) - e.stats.vitality - 2)

    val targetMsg = GameMessage(s"${p.name} uses Hamstring!")
    val dmgMsg = GameMessage(s"${e.name} slows down and takes $dmg damage.")

    // This should be replaced by a generic attack/damage function
    val newEnemy = EnemyOptics.charStats.composeLens(CharStatsOptics.hp)
        .modify(hp => hp - dmg).
      andThen(EnemyOptics.charStats.composeLens(CharStatsOptics.speed)
        .modify(s => Math.min(1, s - 1)))(e)

    val gameMessages = msgs :+ targetMsg :+ dmgMsg

    (p.copy(position = p.position - 100), Option(newEnemy), gameMessages)
  }

  def stunningStrike(p: Player, e: Enemy, msgs: Seq[GameMessage]) = {
    val dmg = Math.max(1, (2 * p.stats.strength) - e.stats.vitality - 3)

    val targetMsg = GameMessage(s"${p.name} uses Stunning Strike!")
    val dmgMsg = GameMessage(s"${e.name} is stunned and takes $dmg damage!")

    // This should be replaced by a generic attack/damage function
    val newEnemy = EnemyOptics.charStats.composeLens(CharStatsOptics.hp)
        .modify(hp => hp - dmg).
      andThen(EnemyOptics.position
        .modify(s => s - 100 - p.stats.strength))(e)
    val gameMessages = msgs :+ targetMsg :+ dmgMsg

    (p.copy(position = p.position - 105), Option(newEnemy), gameMessages)
  }

  def assassinate(p: Player, e: Enemy, msgs: Seq[GameMessage]) = {
    val dmg = Math.max(p.stats.dexterity, e.stats.maxHp - e.stats.currentHp)

    val targetMsg = GameMessage(s"${p.name} uses Assassinate!")
    val dmgMsg = GameMessage(s"${e.name} takes $dmg damage.")

    // This should be replaced by a generic attack/damage function
    val newEnemy = EnemyOptics.charStats.composeLens(CharStatsOptics.hp)
      .modify(hp => hp - dmg)(e)
    val gameMessages = msgs :+ targetMsg :+ dmgMsg

    (p.copy(position = p.position - 100), Option(newEnemy), gameMessages)
  }

  def pain(p: Player, e: Enemy, msgs: Seq[GameMessage]) = {
    val dmg = Math.max(p.stats.intellect / 2, e.stats.currentHp / 2)

    val targetMsg = GameMessage(s"${p.name} uses Pain!")
    val dmgMsg = GameMessage(s"${e.name} convulses in pain and takes $dmg damage!")

    // This should be replaced by a generic attack/damage function
    val newEnemy = EnemyOptics.charStats.composeLens(CharStatsOptics.hp)
      .modify(hp => hp - dmg)(e)
    val gameMessages = msgs :+ targetMsg :+ dmgMsg

    (p.copy(position = p.position - 100), Option(newEnemy), gameMessages)
  }

  def magicMissile(p: Player, e: Enemy, msgs: Seq[GameMessage]) = {
    val dmg = Math.max(1, 2 + p.stats.intellect * 2)

    val targetMsg = GameMessage(s"${p.name} uses Magic Missile!")
    val dmgMsg = GameMessage(s"The missile strikes ${e.name} for $dmg damage!")

    // This should be replaced by a generic attack/damage function
    val newEnemy = EnemyOptics.charStats.composeLens(CharStatsOptics.hp)
      .modify(hp => hp - dmg)(e)
    val gameMessages = msgs :+ targetMsg :+ dmgMsg

    (p.copy(position = p.position - 100), Option(newEnemy), gameMessages)
  }

  def drain(p: Player, e: Enemy, msgs: Seq[GameMessage]) = {
    val dmg = Math.max(1, p.stats.intellect + (e.stats.maxHp / 10))

    val targetMsg = GameMessage(s"${p.name} uses Drain!")
    val dmgMsg = GameMessage(s"${p.name} drains $dmg health from ${e.name}!")

    // This should be replaced by a generic attack/damage function
    val newEnemy = EnemyOptics.charStats.composeLens(CharStatsOptics.hp)
      .modify(hp => hp - dmg)(e)
    val gameMessages = msgs :+ targetMsg :+ dmgMsg

    (p.copy(position = p.position - 100), Option(newEnemy), gameMessages)
  }
}
