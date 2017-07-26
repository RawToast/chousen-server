package chousen.game

import chousen.api.data.{Enemy, GameMessage, GameState, Player}

package object core {
  type Actors = (Player, Set[Enemy])
  type EncounterData = (Player, Set[Enemy], Seq[GameMessage])
  type EncounterUpdate = ((Player, Set[Enemy], Seq[GameMessage])) => (Player, Set[Enemy], Seq[GameMessage])


  type GameOperation = GameState => GameState
  type ForkedGameOperation = GameState => Either[GameState, GameState]
}
