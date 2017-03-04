package chousen.core

import api.data.{Enemy, GameMessage, Player}


object GameOps {
  def update(speed10Player: Player, enemies: Set[Enemy], emptyMessages: Seq[GameMessage]):
  (Player, Set[Enemy], Seq[GameMessage]) = ???
}
