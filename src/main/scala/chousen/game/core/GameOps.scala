package chousen.game.core

import chousen.api.data._

import scala.annotation.tailrec
import scala.util.{Left, Right}


object GameOps extends GameOps {
  override val encOps = EncounterOps
}

trait GameOps {

  val encOps: EncounterOps

  def update(player: Player, enemies: Seq[Enemy], messages: Seq[GameMessage]): EncounterData = {

    def process: EncounterUpdate = encOps.ensureActive _ andThen  encOps.announceActive

    process(Tuple3(player, enemies, messages))
  }

  @scala.annotation.tailrec
  final def updateUntilPlayerIsActive(player: Player, enemies: Seq[Enemy], messages: Seq[GameMessage]): EncounterData = {

    val next: (Player, Seq[Enemy], Seq[GameMessage]) = update(player, enemies, messages)
    encOps.getActive(next) match {
      case Left(_) => next
      case Right(_) => {
        val (p, es, msgs) = EnemyTurnOps.takeTurn(next._1, next._2, next._3)

        updateUntilPlayerIsActive(p, es, msgs)
      }
    }
  }
}


object EnemyTurnOps {

  // This method will use the enemy with the highest position
  def takeTurn(player: Player, enemies: Seq[Enemy], messages: Seq[GameMessage]) = {

    val activeEnemy = enemies.maxBy(_.position)
    val msg1 = GameMessage(s"${activeEnemy.name} attacks ${player.name}!")



    val atkPwr = 3 + activeEnemy.stats.strength + activeEnemy.stats.dexterity
    val defPwr = player.stats.vitality
    val dmg = atkPwr - defPwr

    val playerHp = PlayerOptics.charStats.composeLens(CharStatsOptics.hp)
    val hpLens: (Player) => Player = playerHp.modify(hp => hp - dmg)

    // Message
    val msg2 = GameMessage(s"${activeEnemy.name} deals $dmg to ${player.name}")

    // Then reset
    def reset(e: Enemy) = e.copy(position = e.position - 100)
    import chousen.api.types.Implicits._
    val es = enemies.map(e => if(e ~= activeEnemy) reset(e) else e)

    (hpLens.apply(player), es, messages :+ msg1 :+ msg2)
  }
}




object EncounterOps extends EncounterOps{

  @tailrec
  override def ensureActive(ed: EncounterData): EncounterData = {
    import chousen.api.types.Implicits._

    val (p, es, msgs) = ed
    val (player, enemies) = p.copy(position = p.position + p.stats.speed) ->
      es.map(e => e.copy(position = e.position + e.stats.speed))

    val maxPosition = math.max(player.position, enemies.maxBy(_.position).position)
    lazy val numWithMaxPosition = if (player.position == maxPosition) 1 + enemies.count(_.position == maxPosition)
    else enemies.count(_.position == maxPosition)


    if (maxPosition < 100) ensureActive((player, enemies, msgs))
    else {
      lazy val withPosition = (es: Seq[Enemy]) => es.filter(_.position == maxPosition)
      lazy val enemiesWithPosition = withPosition(enemies)
      lazy val fastestEnemySpeed = enemiesWithPosition.maxBy(_.stats.speed).stats.speed

      numWithMaxPosition match {
        case 1 => (player, enemies, msgs)
        case 2 if player.position == maxPosition =>
          if (player.stats.speed != fastestEnemySpeed) ensureActive((player, enemies, msgs))
          else {
            val incEnemies: Seq[Enemy] = enemies.map(e =>
              if (e ~= enemies.maxBy(_.stats.speed)) {
                e.copy(position = e.position + 1)
              } else e)

            ensureActive(Tuple3(player, incEnemies, msgs))
          }
        case _ if player.position == maxPosition =>
          if (player.stats.speed != fastestEnemySpeed) ensureActive((player, enemies, msgs))
          else {
            val fastestSpeeds = enemiesWithPosition.filter(_.stats.speed == fastestEnemySpeed)

            // Remove enemies without max speed (recurse)
            if (enemiesWithPosition.size > fastestSpeeds.size) {
              ensureActive(Tuple3(player, enemies, msgs))
            } else {
              // Player speed equals fastest enemy (a min of 2 have same speed)
              // All same speed
              val chosenOne: Enemy = enemiesWithPosition.maxBy(_.id)
              val nextEnemies: Seq[Enemy] =
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

  override def announceActive(ed: EncounterData): EncounterData = {
    val (player, enemies, msgs) = ed

    val fastestEnemy = enemies.maxBy(_.position)

    val message = {
      if (player.position > fastestEnemy.position) GameMessage(s"${player.name}'s turn!")
      else GameMessage(s"${fastestEnemy.name}'s turn!")
    }

    (player, enemies, msgs :+ message)
  }

  override def getActive(x: EncounterData): Either[Player, Enemy] = {
    val (p: Player, es: Seq[Enemy], _) = x
    val maxPosition = math.max(p.position, es.maxBy(_.position).position)

    if (p.position == maxPosition) Left(p)
    else Right(es.maxBy(_.position))
  }
}

trait EncounterOps {
  def ensureActive(ed: EncounterData): EncounterData

  def announceActive(ed: EncounterData): EncounterData

  def getActive(x: EncounterData): Either[Player, Enemy]
}
