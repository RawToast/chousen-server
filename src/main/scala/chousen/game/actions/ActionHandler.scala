package chousen.game.actions

import chousen.Optics
import chousen.api.data.GameState
import chousen.game.core.turn.PostTurnOps

trait ActionHandler {

  def handleDead: (GameState) => GameState =
    Optics.EncounterLens.modify(PostTurnOps.handleDead)
}

