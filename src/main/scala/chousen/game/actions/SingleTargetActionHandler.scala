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
      case Mammonite => mammonite
      case Bankruptcy => bankruptcy

      case QuickAttack => quickAttack
      case Assassinate => assassinate
      case ToxicShiv => toxicShiv

      case Pain => pain
      case MagicMissile => magicMissile
      case Ember => ember
      case Drain => drain
    }


  def ability(p: Player, e: Enemy, msgs: Seq[GameMessage])(
    useMsg: (String, String) => String = (p: String, e: String) => s"$p uses non-descript ability on $e.",
    damageMsg: (String, Int) => String = (e: String, d: Int) => s"$e takes $d damage.",
    multi: Multipliers = new Multipliers(),
    bonusDamage: Int = 0,
    damageCalc: (Player, Enemy, Multipliers) => Int = damageCalculator.calculatePlayerDamage,
    enemyEffect: Enemy => Enemy = e => e,
    speed: Int = STANDARD,
    goldCost: Int=0): (Player, Option[Enemy], Seq[GameMessage]) = {

    val sePlayer = damageCalculator.sc.calculate(p)

    val dmg = damageCalc(p, e, multi) + bonusDamage

    val targetMsg = GameMessage(useMsg(sePlayer.name, e.name))
    val dmgMsg = GameMessage(damageMsg(e.name, dmg))

    val newEnemy = EnemyStatsLens.composeLens(HpLens)
      .modify(hp => hp - dmg)
      .andThen(enemyEffect)(e)
    val gameMessages = msgs :+ targetMsg :+ dmgMsg

    val updatedPlayer = p.copy(position = calculatePosition(sePlayer, speed).position)
    val finalPlayer = updatedPlayer.copy(gold = updatedPlayer.gold - goldCost)
    (finalPlayer, Option(newEnemy), gameMessages)
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
    enemyEffect = EnemyStatusLens.modify(_ :+ StatusBuilder.makeBurn(damageCalculator.sc.calculate(p).stats.strength / 2)),
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

    val updatedPlayer = p.copy(position = calculatePosition(sePlayer, SLUGGISH, sePlayer.stats.strength / 4).position)

    (updatedPlayer, Option(newEnemy), gameMessages)
  }

  def mammonite(p: Player, e: Enemy, msgs: Seq[GameMessage]): ActionUpdate = ability(p, e, msgs)(
    useMsg = (p, e) => s"$p uses Mammonite!",
    multi = Multipliers.strengthSkill,
    bonusDamage = p.experience.level + 10,
    speed = ENHANCED,
  )

  def bankruptcy(p: Player, e: Enemy, msgs: Seq[GameMessage]): ActionUpdate = ability(p, e, msgs)(
    useMsg = (p, e) => s"$p hits $e with bankruptcy!",
    multi = Multipliers.highStrengthSkill,
    bonusDamage = p.experience.level + p.gold,
    goldCost = p.gold / 2,
    enemyEffect = e => e.copy(position = e.position - (p.gold / 2)),
    speed = ENHANCED
  )


  // Dex
  def quickAttack(p: Player, e: Enemy, msgs: Seq[GameMessage]) = ability(p, e, msgs)(
    useMsg = (p, _) => s"$p uses Quick Attack!",
    multi = Multipliers.dexteritySkill,
    speed = QUICK - (p.stats.dexterity / 2),
    bonusDamage = p.experience.level)


  def toxicShiv(p: Player, e: Enemy, msgs: Seq[GameMessage]) = ability(p, e, msgs)(
    useMsg = (p, _) => s"$p uses Toxic Shiv!",
    damageMsg = (e, d) => s"$e is dazed, poisoned and takes $d damage!",
    multi = Multipliers.lowDexteritySkill,
    enemyEffect = EnemyStatusLens.modify(_ :+ StatusBuilder.makeSlow(1, turns = 1) :+
      StatusBuilder.makePoison((p.experience.level + damageCalculator.sc.calculate(p).stats.dexterity) / 2))
  )

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

    val updatedPlayer = p.copy(position = calculatePosition(sePlayer).position)

    (updatedPlayer, Option(newEnemy), gameMessages)
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

    val updatedPlayer = p.copy(position = calculatePosition(sePlayer).position)

    (updatedPlayer, Option(newEnemy), gameMessages)
  }

  def magicMissile(p: Player, e: Enemy, msgs: Seq[GameMessage]) = ability(p, e, msgs)(
    useMsg = (p, _) => s"$p uses Magic Missile!",
    damageMsg = (e, d) => s"The missile strikes $e for $d damage!",
    bonusDamage = 2,
    multi = Multipliers.intellectSkill,
    damageCalc = damageCalculator.calculatePlayerMagicDamage)


  def ember(p: Player, e: Enemy, msgs: Seq[GameMessage]) = ability(p, e, msgs)(
    useMsg = (p, _) => s"$p uses Ember!",
    damageMsg = (e, d) => s"$e is burnt by the flames for $d damage!",
    multi = Multipliers.lowIntellectSkill,
    enemyEffect = EnemyStatusLens.modify(ss => ss :+
      StatusBuilder.makeBurn((2 * ss.count(_.effect == Burn)) + (damageCalculator.sc.calculate(p).stats.intellect / 2))),
    bonusDamage =  Math.max(0, e.status.count(_.effect == Burn)),
    damageCalc = damageCalculator.calculatePlayerMagicDamage
  )

  def drain(p: Player, e: Enemy, msgs: Seq[GameMessage]) = {

    val targetMsg = GameMessage(s"${p.name} uses Drain!")

    val dmg = Math.max(1,Math.min(p.stats.maxHp - p.stats.currentHp,
      p.stats.maxHp - p.stats.currentHp + damageCalculator.sc.calculate(p).stats.intellect - 20))

    val newEnemy = EnemyStatsLens.composeLens(HpLens)
      .modify(_ - dmg)(e)

    val dmgMsg = GameMessage(s"${p.name} drains $dmg health from ${e.name}!")
    val gameMessages = msgs :+ targetMsg :+ dmgMsg


    (calculatePosition(PlayerHealthLens.set(Math.min(e.stats.currentHp + dmg, p.stats.maxHp))(p)),
      Option(newEnemy), gameMessages)
  }
}
