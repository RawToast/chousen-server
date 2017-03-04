package chousen.core

import api.data.{Enemy, GameMessage, Player}

import scala.annotation.tailrec


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

    val numWithPosition = if (player.position >= 100) 1 + enemies.count(_.position >= 100)
    else enemies.count(_.position >= 100)

    numWithPosition match {
      case 0 => ensureActive(Tuple3(player, enemies, msgs))
      case 1 => (player, enemies, msgs)
      case _ => (player, enemies, msgs) //TODO handle
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
