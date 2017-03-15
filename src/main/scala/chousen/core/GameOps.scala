package chousen.core

import api.data.{Enemy, GameMessage, Player}

import scala.annotation.tailrec
import scala.util.Random


object GameOps {
  type Actors = (Player, Set[Enemy])
  type EncounterData = (Player, Set[Enemy], Seq[GameMessage])
  type EncounterUpdate = ((Player, Set[Enemy], Seq[GameMessage])) => (Player, Set[Enemy], Seq[GameMessage])

  def update(player: Player, enemies: Set[Enemy], messages: Seq[GameMessage]):
  (Player, Set[Enemy], Seq[GameMessage]) = {

    def process: EncounterUpdate = ensureActive _ andThen announceActive

    process(Tuple3(player, enemies, messages))
  }

  @tailrec
  def ensureActive(encounterData: EncounterData): EncounterData = {
    val (p, es, msgs) = encounterData
    val (player, enemies) = p.copy(position = p.position + p.stats.speed) ->
      es.map(e => e.copy(position = e.position + e.stats.speed))

    val maxPosition = math.max(player.position, enemies.maxBy(_.position).position)
    lazy val numWithMaxPosition = if (player.position == maxPosition) 1 + enemies.count(_.position == maxPosition)
      else enemies.count(_.position == maxPosition)


    if (maxPosition < 100) ensureActive((player, enemies, msgs))
    else {
      lazy val withPosition = (es:Set[Enemy]) => es.filter(_.position == maxPosition)
      lazy val enemiesWithPosition = withPosition(enemies)


      numWithMaxPosition match {
        case 0 => ensureActive(Tuple3(player, enemies, msgs))
        case 1 => (player, enemies, msgs)
        case 2 if player.position == maxPosition =>
          if (player.stats.speed != enemiesWithPosition.maxBy(_.stats.speed).stats.speed) ensureActive((player, enemies, msgs))
          else {
            val incEnemies: Set[Enemy] = enemies.map(e =>
              if (e.id == enemies.maxBy(_.stats.speed).id) {e.copy(position = e.position + 1) } else e)

            ensureActive(Tuple3(player, incEnemies, msgs))
          }
        case _ if player.position == maxPosition =>
          if (player.stats.speed != enemiesWithPosition.maxBy(_.stats.speed).stats.speed) ensureActive((player, enemies, msgs))
          else {


           ???
          }
        case _ =>
          enemiesWithPosition.map(e => e.copy(position = e.stats.speed + e.position))
          val fastestSpeeds = enemiesWithPosition.filter(_.stats.speed == enemiesWithPosition.maxBy(_.stats.speed))

          fastestSpeeds.size match {
            case 1 => ensureActive((player, enemies, msgs))
            case _ =>
              if (fastestSpeeds.size < enemiesWithPosition.size) ensureActive((player, enemies, msgs))
              else {
                val chosenOne: Enemy = Random.shuffle(enemiesWithPosition).head
                val nextEnemies = enemies.map(e => if (e.id == chosenOne.id) e.copy(position = e.position + 1)
                else e)
                ensureActive((player, nextEnemies, msgs))
              }
          }
      }
    }
  }

  private def announceActive(encounterData: EncounterData): EncounterData = {
    val (player, enemies, msgs) = encounterData

    val fastestEnemy = enemies.maxBy(_.position)

    val message = {
      if (player.position > fastestEnemy.position) GameMessage(s"${player.name}'s turn!")
      else GameMessage(s"${fastestEnemy.name}'s turn!")
    }

    (player, enemies, msgs :+ message)
  }
}
