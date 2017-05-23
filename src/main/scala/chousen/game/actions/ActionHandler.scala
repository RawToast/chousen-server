package chousen.game.actions

import chousen.api.data.{GameMessage, GameState}
import chousen.game.core.GameStateOptics

trait ActionHandler {
  def handleDead: (GameState) => GameState = GameStateOptics.EncounterLens.modify {
    case (p, es, msgs) =>

      val aliveEnemies = es.filter(_.stats.currentHp > 0)
      val newMessages = es.filter(_.stats.currentHp < 0)
        .toSeq
        .map(e => GameMessage(s"${e.name} dies"))

      (p, aliveEnemies, msgs ++: newMessages)
  }
}
