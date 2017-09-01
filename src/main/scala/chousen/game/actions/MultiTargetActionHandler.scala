package chousen.game.actions

import java.util.UUID

import chousen.api.data._
import chousen.game.core.turn.PositionCalculator.calculatePosition
import chousen.game.core.GameStateOptics._
import chousen.game.status.StatusBuilder

class MultiTargetActionHandler(dc: DamageCalculator) extends ActionHandler {

  def handle(targetId: Set[UUID], action: MultiAction): (GameState) => GameState = (gs: GameState) => {

    val message = action.toString.head + ('A' to 'Z').foldLeft(action.toString.tail){case (str: String, c: Char) =>
        str.replace(s"$c", s" $c")
    }

    val targetMsg = GameMessage(s"${gs.player.name} uses $message!")
    val gsWithMessage = gs.copy(messages = gs.messages ++ Seq(targetMsg))

    val newState = handleDead(targetId.foldLeft(gsWithMessage) { case (gs: GameState, id: UUID) =>
      targettedLens(id).modify {
        case (p, es, msgs) =>
          es match {
            case Some(e) => actions(action)(p, e, msgs)
            case None => (p, es, msgs)
          }
      }.apply(gs)
    })

    if (gsWithMessage == newState) gs
    else {

      val finalState = newState

      PlayerLens.modify(p => calculatePosition(p))
      .andThen(handleDead)
      .apply(finalState)
    }
  }

  private def actions(action: MultiAction): (Player, Enemy, Seq[GameMessage]) => (Player, Option[Enemy], Seq[GameMessage]) = {
    action match {
      case GroundStrike => groundStrike
      case WindStrike => windStrike
      case Fireball => fireball
      case PotionOfFlames => flames
      case Extinguish => extinguish
    }
  }

  def groundStrike(p: Player, e: Enemy, msgs: Seq[GameMessage]) = {
    val dmg = dc.calculatePlayerDamage(p, e,
      Multipliers.builder.strMulti(Multipliers.lowMulti).maxMulti(Multipliers.multiTarget).m)

    val sePlayer = dc.sc.calculate(p)
    val gameMessages = msgs :+ GameMessage(s"${e.name} takes $dmg damage.")

    // This should be replaced by a generic attack/damage function
    val newE = EnemyOptics.EnemyStatsLens.composeLens(CharStatsOptics.HpLens).modify(hp => hp - dmg)
      .andThen(EnemyOptics.EnemyPosition.modify(ep => ep - 10 - (sePlayer.stats.strength / 2)))
      .apply(e)

    (p, Option(newE), gameMessages)
  }

  def windStrike(p: Player, e: Enemy, msgs: Seq[GameMessage]) = {

    val dmg = dc.calculatePlayerDamage(p, e,
      Multipliers.builder
        .dexMulti(Multipliers.medMulti)
        .intMulti(Multipliers.lowMulti)
        .maxMulti(Multipliers.multiTarget).m) + (dc.sc.calculate(p).stats.intellect / 4)

    val gameMessages = msgs :+ GameMessage(s"${e.name} takes $dmg damage.")

    // This should be replaced by a generic attack/damage function
    val newE = EnemyOptics.EnemyStatsLens.composeLens(CharStatsOptics.HpLens)
      .modify(hp => hp - dmg)(e)

    (p, Option(newE), gameMessages)
  }

  def fireball(p: Player, e: Enemy, msgs: Seq[GameMessage]) = {

    val sePlayer = dc.sc.calculate(p)
    val magicDmg = dc.calculatePlayerMagicDamage(p, e,
      Multipliers.builder.intMulti(Multipliers.maxMulti).maxMulti(Multipliers.multiTarget).m)

    val dmg = Math.max(1, magicDmg + (p.experience.level * 2))
    val gameMessages = msgs :+ GameMessage(s"${e.name} is engulfed by flames and takes $dmg damage.")

    val newE = EnemyOptics.EnemyStatsLens.composeLens(CharStatsOptics.HpLens).modify(hp => hp - dmg)
      .andThen(EnemyOptics.EnemyStatusLens.modify(_ :+ StatusBuilder.makeBurn(sePlayer.stats.intellect / 8)))(e)

    (p, Option(newE), gameMessages)
  }

  def extinguish(p: Player, e: Enemy, msgs: Seq[GameMessage]) = {

    if (e.status.exists(_.effect == Burn)) {
      val magicDmg = dc.calculatePlayerMagicDamage(p, e,
        Multipliers.builder.intMulti(Multipliers.lowMulti).maxMulti(Multipliers.multiTarget).m)

      val missingHp = e.stats.maxHp - e.stats.currentHp

      val dmg = Math.max(1, magicDmg + (missingHp / 3)) +
        e.status.filter(_.effect == Burn)
          .map(e => e.amount.map(_ * e.turns).getOrElse(0))
          .sum

      val gameMessages = msgs :+ GameMessage(s"${e.name} is magically extinguished and takes $dmg damage.")

      val newE = EnemyOptics.EnemyStatsLens.composeLens(CharStatsOptics.HpLens).modify(hp => hp - dmg)
        .andThen(EnemyOptics.EnemyStatusLens.modify(_.filterNot(_.effect == Burn)))(e)

      (p, Option(newE), gameMessages)
    }
    else (p, Option(e), msgs)
  }

  def flames(p: Player, e: Enemy, msgs: Seq[GameMessage]) = {

    val gameMessages = msgs :+ GameMessage(s"${e.name} is burnt by the flames.")

    val newE = EnemyOptics.EnemyStatusLens.modify(_ :+ StatusBuilder.makeBurn(4, turns = 10))(e)

    (p, Option(newE), gameMessages)
  }
}
