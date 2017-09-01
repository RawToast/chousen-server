package chousen.game.actions

import java.util.UUID

import chousen.Optics._
import chousen.api.data._
import chousen.game.actions.Multipliers.{builder, highMulti, medMulti}
import chousen.game.core.turn.PositionCalculator._
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
      case BurningHammer => burningHammer

      case QuickAttack => quickAttack
      case Assassinate => assassinate

      case Pain => pain
      case MagicMissile => magicMissile
    }

  def attackAbility(calc: (Player, Enemy, Multipliers) => Int ) = {

  }

  def ability(p: Player, e: Enemy, msgs: Seq[GameMessage])(
    useMsg: (String, String) => String = (p: String, e: String) => s"$p uses non-descript ability on $e.",
    damageMsg: (String, Int) => String = (e: String, d: Int) => s"$e takes $d damage.",
    multi: Multipliers = new Multipliers(),
    bonusDamage: Int = 0,
    damageCalc: (Player, Enemy, Multipliers) => Int = damageCalculator.calculatePlayerDamage,
    enemyEffect: Enemy => Enemy = e => e,
    speed: Int = STANDARD): (Player, Option[Enemy], Seq[GameMessage]) = {

    val sePlayer = damageCalculator.sc.calculate(p)

    val dmg = damageCalc(p, e, multi) + bonusDamage

    val targetMsg = GameMessage(useMsg(sePlayer.name, e.name))
    val dmgMsg = GameMessage(damageMsg(e.name, dmg))

    val newEnemy = EnemyStatsLens.composeLens(HpLens)
      .modify(hp => hp - dmg)
      .andThen(enemyEffect)(e)
    val gameMessages = msgs :+ targetMsg :+ dmgMsg

    val updatedPlayer = p.copy(position = calculatePosition(sePlayer, speed).position)
    (updatedPlayer, Option(newEnemy), gameMessages)
  }

  type ActionUpdate = (Player, Option[Enemy], Seq[GameMessage])

  def crushingBlow(p: Player, e: Enemy, msgs: Seq[GameMessage]): ActionUpdate = ability(p, e, msgs)(
      useMsg = (p, e) => s"$p lands a crushing blow on $e!",
      multi = builder.strMulti(highMulti).dexMulti(medMulti).m,
      speed = SLUGGISH
    )

  def stunningStrike(p: Player, e: Enemy, msgs: Seq[GameMessage]) = ability(p, e, msgs)(
    useMsg = (p, _) => s"$p uses Stunning Strike!",
    damageMsg = (e, d) => s"$e is dazed and takes $d damage!",
    multi = Multipliers.lowStrengthSkill,
    enemyEffect = EnemyPosition.modify(s => s - 70 - p.stats.strength)
      .andThen(EnemyStatusLens.modify(_ :+ StatusBuilder.makeSlow(2)))
  )

  def counter(p: Player, e: Enemy, msgs: Seq[GameMessage]) = ability(p, e, msgs)(
    useMsg = (p, _) => s"$p uses Counter!",
    damageMsg = (e, d) => s"$e is countered and takes $d damage.",
    multi = Multipliers.strengthSkill,
    bonusDamage = e.stats.strength
  )

  def burningHammer(p: Player, e: Enemy, msgs: Seq[GameMessage]) = ability(p, e, msgs)(
    useMsg = (p, _) => s"$p uses Burning Hammer!",
    damageMsg = (e, d) => s"$e is burnt and takes $d damage!",
    multi = Multipliers.lowStrengthSkill,
    enemyEffect = EnemyStatusLens.modify(_ :+ StatusBuilder.makeBurn(p.stats.strength / 2)),
    bonusDamage = if (e.status.exists(_.effect == Burn)) {Math.max(3, (e.stats.maxHp - e.stats.currentHp) / 8)} else 0
  )

  def destruction(p: Player, e: Enemy, msgs: Seq[GameMessage]) = {
    val dmg = damageCalculator.calculatePlayerDamage(p, e, Multipliers.lowStrengthSkill)
    val sePlayer = damageCalculator.sc.calculate(p)

    val newEnemy = LensUtil.duoLens(EnemyStatsLens.composeLens(HpLens), EnemyStatsLens.composeLens(VitalityLens))
      .modify { case (hp, vit) => hp - dmg -> Math.max(1, vit - (sePlayer.stats.strength / 2)) }
      .apply(e)

    val vitLoss = e.stats.vitality - newEnemy.stats.vitality

    val gameMessages = if (vitLoss > 0) {
      val targetMsg = GameMessage(s"${p.name} lands a destructive blow on ${e.name}!")

      val dmgMsg = GameMessage(s"${e.name}'s loses $vitLoss Vitality and takes $dmg damage.")
      msgs :+ targetMsg :+ dmgMsg
    } else msgs :+ GameMessage(s"${p.name} lands a destructive blow on ${e.name} and deals $dmg damage!")

    (calculatePosition(p,
      SLUGGISH,
      sePlayer.stats.strength / 4), Option(newEnemy), gameMessages)
  }


  // Dex
  def quickAttack(p: Player, e: Enemy, msgs: Seq[GameMessage]) = ability(p, e, msgs)(
    useMsg = (p, _) => s"$p uses Quick Attack!",
    multi = Multipliers.dexteritySkill,
    speed = QUICK - (p.stats.dexterity / 2))

  def assassinate(p: Player, e: Enemy, msgs: Seq[GameMessage]): (Player, Option[Enemy], Seq[GameMessage]) = {

    val sePlayer = damageCalculator.sc.calculate(p)
    val seEnemy = damageCalculator.sc.calculate(e)
    val basicDamage = damageCalculator.calculatePlayerDamage(p, e)

    val hpDiff = e.stats.maxHp - e.stats.currentHp
    val reduce = Math.min(1d, sePlayer.stats.dexterity.toDouble / (2 * seEnemy.stats.vitality.toDouble))

    val dmg = Math.max(sePlayer.stats.dexterity / 4d, hpDiff * reduce).toInt + (basicDamage / 4)

    val targetMsg = GameMessage(s"${p.name} uses Assassinate!")
    val dmgMsg = GameMessage(s"${e.name} takes $dmg damage.")

    val newEnemy = EnemyStatsLens.composeLens(HpLens)
      .modify(hp => hp - dmg)(e)
    val gameMessages = msgs :+ targetMsg :+ dmgMsg

    (calculatePosition(p), Option(newEnemy), gameMessages)
  }


  // Int
  def pain(p: Player, e: Enemy, msgs: Seq[GameMessage]) = {

    val sePlayer = damageCalculator.sc.calculate(p)
    val seEnemy = damageCalculator.sc.calculate(e)

    val dmg = Math.max(p.stats.intellect / 2d,
      e.stats.currentHp * Math.min(0.5, sePlayer.stats.intellect.toDouble / (2 * seEnemy.stats.vitality.toDouble))).toInt

    val targetMsg = GameMessage(s"${p.name} uses Pain!")
    val dmgMsg = GameMessage(s"${e.name} convulses in pain and takes $dmg damage!")

    val newEnemy = EnemyStatsLens.composeLens(HpLens)
      .modify(hp => hp - dmg)(e)
    val gameMessages = msgs :+ targetMsg :+ dmgMsg

    (calculatePosition(p), Option(newEnemy), gameMessages)
  }

  def magicMissile2(p: Player, e: Enemy, msgs: Seq[GameMessage]) = ability(p, e, msgs)(
    useMsg = (p, _) => s"$p uses Magic Missile!",
    damageMsg = (e, d) => s"The missile strikes $e for $d damage!",
    multi = Multipliers.intellectSkill,
    damageCalc = damageCalculator.calculatePlayerMagicDamage)

  def magicMissile(p: Player, e: Enemy, msgs: Seq[GameMessage]) = {
    val magicDmg = damageCalculator.calculatePlayerMagicDamage(p, e, Multipliers.intellectSkill)

    val dmg = Math.max(1, 2 + magicDmg)

    val targetMsg = GameMessage(s"${p.name} uses Magic Missile!")
    val dmgMsg = GameMessage(s"The missile strikes ${e.name} for $dmg damage!")

    // This should be replaced by a generic attack/damage function
    val newEnemy = EnemyStatsLens.composeLens(HpLens)
      .modify(hp => hp - dmg)(e)
    val gameMessages = msgs :+ targetMsg :+ dmgMsg

    (calculatePosition(p), Option(newEnemy), gameMessages)
  }
}
