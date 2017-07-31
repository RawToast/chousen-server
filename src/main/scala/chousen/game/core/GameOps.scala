package chousen.game.core

import chousen.api.data._
import chousen.game.actions.DamageCalculator
import chousen.game.status.StatusCalculator

import scala.annotation.tailrec
import scala.util.{Left, Right}


object GameOps extends GameOps(new EncounterOp(new StatusCalculator), new DamageCalculator(new StatusCalculator))

abstract class GameOps(encOps: EncounterOps, damageCalculator: DamageCalculator) {

  final def updateUntilPlayerIsActive(ed: EncounterData): EncounterData =
    updateUntilPlayerIsActive(ed._1, ed._2, ed._3)


  @scala.annotation.tailrec
  final def updateUntilPlayerIsActive(player: Player, enemies: Set[Enemy], messages: Seq[GameMessage]): EncounterData = {
    val next: (Player, Set[Enemy], Seq[GameMessage]) = update(player, enemies, messages)
    encOps.getActive(next) match {
      case Left(_) => next
      case Right(_) =>
        val (p, es, msgs) = EnemyTurnOps.takeTurn(next._1, next._2, next._3)(damageCalculator)

        updateUntilPlayerIsActive(p, es, msgs)
    }
  }

  // Note Left side is inactive, Right is active
  def isGameActive(ed: EncounterData): Boolean = {
    val (player, enemies, _) = ed
    if (0 >= player.stats.currentHp || enemies.isEmpty) false
    else true
  }


  def update(player: Player, enemies: Set[Enemy], messages: Seq[GameMessage]): EncounterData = {

    def process: EncounterUpdate = encOps.ensureActive _ andThen encOps.announceActive

    process(Tuple3(player, enemies, messages))
  }
}


object EnemyTurnOps {

  // This method will use the enemy with the highest position
  def takeTurn(player: Player, enemies: Set[Enemy], messages: Seq[GameMessage])(dc: DamageCalculator): (Player, Set[Enemy], Seq[GameMessage]) = {

    val activeEnemy = enemies.maxBy(_.position)
    val dmg = dc.calculateEnemyDamage(activeEnemy, player)


    val playerHp = PlayerOptics.PlayerCharStatsLens.composeLens(CharStatsOptics.HpLens)
    val hpLens: (Player) => Player = playerHp.modify(hp => hp - dmg)

    // Message
    val attackMessage = GameMessage(s"${activeEnemy.name} attacks ${player.name} for $dmg damage.")

    // Then reset
    def reset(e: Enemy) = e.copy(position = e.position - 100)
    import chousen.Implicits._
    val es = enemies.map(e => if (e ~= activeEnemy) reset(e) else e)

    (hpLens.apply(player), es, messages :+ attackMessage)
  }
}


final class EncounterOp(sc: StatusCalculator) extends EncounterOps {

  @tailrec
  override def ensureActive(encounterData: EncounterData): EncounterData = {
    import chousen.Implicits._

    val (p, es, msgs) = encounterData
    val sePlayer = sc.calculate(p)

    val (player, enemies) = p.copy(position = p.position + sePlayer.stats.speed) ->
      es.map(e => e.copy(position = e.position + e.stats.speed))

    val maxPosition = math.max(player.position, enemies.maxBy(_.position).position)
    lazy val numWithMaxPosition = if (player.position == maxPosition) 1 + enemies.count(_.position == maxPosition)
    else enemies.count(_.position == maxPosition)


    if (maxPosition < 100) ensureActive((player, enemies, msgs))
    else {
      lazy val withPosition = (es: Set[Enemy]) => es.filter(_.position >= maxPosition)
      lazy val enemiesWithPosition = withPosition(enemies)
      lazy val fastestEnemySpeed = enemiesWithPosition.maxBy(_.stats.speed).stats.speed

      numWithMaxPosition match {
        case 1 => (player, enemies, msgs)
        case 2 if player.position == maxPosition =>
          if (sePlayer.stats.speed != fastestEnemySpeed) ensureActive((player, enemies, msgs))
          else {
            val incEnemies: Set[Enemy] = enemies.map(e =>
              if (e ~= enemies.maxBy(_.position)) {
                e.copy(position = e.position + 1)
              } else e)

            ensureActive(Tuple3(player, incEnemies, msgs))
          }
        case _ if player.position == maxPosition =>
          if (sePlayer.stats.speed != fastestEnemySpeed) ensureActive((player, enemies, msgs))
          else {
            val fastestSpeeds = enemiesWithPosition.filter(_.stats.speed == fastestEnemySpeed)

            // Remove enemies without max speed (recurse)
            if (enemiesWithPosition.size > fastestSpeeds.size) {
              ensureActive(Tuple3(player, enemies, msgs))
            } else {
              // Player speed equals fastest enemy (a min of 2 have same speed)
              // All same speed
              val chosenOne: Enemy = enemiesWithPosition.maxBy(_.id)
              val nextEnemies: Set[Enemy] =
                enemies.map(e => if (e ~= chosenOne) e.copy(position = e.position + 1) else e)

              ensureActive(Tuple3(player, nextEnemies, msgs))
            }
          }
        case _ =>
          enemiesWithPosition.map(e => e.copy(position = e.stats.speed + e.position))
          val fastestSpeeds = enemiesWithPosition.filter(_.stats.speed == fastestEnemySpeed)

          fastestSpeeds.size match {
            case 1 => ensureActive((player, enemies, msgs))
            case _ =>
              if (fastestSpeeds.size < enemiesWithPosition.size) ensureActive((player, enemies, msgs))
              else {
                val chosenOne: Enemy = enemiesWithPosition.maxBy(_.id)
                val nextEnemies = enemies.map(e => if (e ~= chosenOne) e.copy(position = e.position + 1)
                else e)
                ensureActive((player, nextEnemies, msgs))
              }
          }
      }
    }
  }

  override def announceActive(encounterData: EncounterData): EncounterData = {
    val (player, enemies, msgs) = encounterData

    val fastestEnemy = enemies.maxBy(_.position)

    val newMessages = if (player.position > fastestEnemy.position) msgs :+GameMessage(s"${player.name}'s turn.")
        else msgs
    (player, enemies, newMessages)
  }

  override def getActive(encounterData: EncounterData): Either[Player, Enemy] = {
    val (p: Player, es: Set[Enemy], _) = encounterData
    val maxPosition = math.max(p.position, es.maxBy(_.position).position)

    if (p.position == maxPosition) Left(p)
    else Right(es.maxBy(_.position))
  }
}

trait EncounterOps {
  def ensureActive(encounterData: EncounterData): EncounterData

  def announceActive(encounterData: EncounterData): EncounterData

  def getActive(encounterData: EncounterData): Either[Player, Enemy]
}

