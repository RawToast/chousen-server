package chousen.engine

import chousen.Actors

case class State(playerAlive: Boolean, actors: Actors)

object State {
  def createFromActors(actors: Actors) = {
    val playerAlive: Boolean = if (actors.actor.isPlayer) true
    else actors.cast
      .find(cm => cm.isPlayer)
      .map(pc => pc.currentHp > 0).get

    State(playerAlive, actors)
  }
}
