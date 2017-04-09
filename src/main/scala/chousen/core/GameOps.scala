package chousen.core

import api.data.{Enemy, GameMessage, Player}

import scala.annotation.tailrec

object GameOps extends GameOps {
  override val encOps = EncounterOps
}



trait GameOps {

  val encOps: EncounterOps

  def update(player: Player, enemies: Set[Enemy], messages: Seq[GameMessage]):
  (Player, Set[Enemy], Seq[GameMessage]) = {

    def process: EncounterUpdate = encOps.ensureActive _ andThen encOps.announceActive

    process(Tuple3(player, enemies, messages))
  }
}


object EncounterOps extends EncounterOps{

  @tailrec
  override def ensureActive(ed: EncounterData): EncounterData = {
    import api.types.Implicits._

    val (p, es, msgs) = ed
    val (player, enemies) = p.copy(position = p.position + p.stats.speed) ->
      es.map(e => e.copy(position = e.position + e.stats.speed))

    val maxPosition = math.max(player.position, enemies.maxBy(_.position).position)
    lazy val numWithMaxPosition = if (player.position == maxPosition) 1 + enemies.count(_.position == maxPosition)
    else enemies.count(_.position == maxPosition)


    if (maxPosition < 100) ensureActive((player, enemies, msgs))
    else {
      lazy val withPosition = (es: Set[Enemy]) => es.filter(_.position == maxPosition)
      lazy val enemiesWithPosition = withPosition(enemies)
      lazy val fastestEnemySpeed = enemiesWithPosition.maxBy(_.stats.speed).stats.speed

      numWithMaxPosition match {
        case 0 => ensureActive(Tuple3(player, enemies, msgs))
        case 1 => (player, enemies, msgs)
        case 2 if player.position == maxPosition =>
          if (player.stats.speed != fastestEnemySpeed) ensureActive((player, enemies, msgs))
          else {
            val incEnemies: Set[Enemy] = enemies.map(e =>
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

  override def announceActive(ed: EncounterData): EncounterData = {
    val (player, enemies, msgs) = ed

    val fastestEnemy = enemies.maxBy(_.position)

    val message = {
      if (player.position > fastestEnemy.position) GameMessage(s"${player.name}'s turn!")
      else GameMessage(s"${fastestEnemy.name}'s turn!")
    }

    (player, enemies, msgs :+ message)
  }

}

trait EncounterOps {
  def ensureActive(ed: EncounterData): EncounterData

  def announceActive(ed: EncounterData): EncounterData
}

