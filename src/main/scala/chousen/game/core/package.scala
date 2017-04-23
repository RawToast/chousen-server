package chousen.game

import chousen.api.data.{Enemy, GameMessage, Player}

package object core {
  type Actors = (Player, Seq[Enemy])
  type EncounterData = (Player, Seq[Enemy], Seq[GameMessage])
  type EncounterUpdate = ((Player, Seq[Enemy], Seq[GameMessage])) => (Player, Seq[Enemy], Seq[GameMessage])

}
