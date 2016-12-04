package chousen.engine

import chousen.Cast

case class State(playerAlive: Boolean, actors: Cast)

object State {
  def createFromActors(actors: Cast) = {
    val playerAlive = actors.player.isAlive

    State(playerAlive, actors)
  }
}
