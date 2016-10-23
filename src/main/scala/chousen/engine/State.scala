package chousen.engine

import chousen.Actors

case class State(playerAlive: Boolean, actors: Actors)

object State {
  def createFromActors(actors: Actors) = {
    val playerAlive = if (actors.actor.isPlayer) Option(true)
    else actors.cast
      .find(cm => cm.isPlayer)
      .map(pc => pc.stats.currentHp > 0)

    State(playerAlive.getOrElse(false), actors)
  }
}
