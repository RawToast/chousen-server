package chousen.engine

import chousen.Cast
import chousen.cards.DeckManager

case class State(playerAlive: Boolean, actors: Cast)

object State {
  def createFromActors(actors: Cast) = {
    val playerAlive = if (actors.actor.isPlayer) Option(true)
    else actors.cast
      .find(cm => cm.isPlayer)
      .map(pc => pc.stats.currentHp > 0)

    State(playerAlive.getOrElse(false), actors)
  }
}
