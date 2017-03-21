package api.core

import java.util.UUID

import api.data.GameState
import chousen.core.old.Game
import io.finch.{NotFound, Output}

trait GameAccess {
  def withGame(id: UUID)(f: Game => Output[GameState]): Output[GameState]
}

trait MappedGameAccess extends GameAccess {
  var store = Map.empty[UUID, Game]

  def withGame(id: UUID)(f: Game => Output[GameState]): Output[GameState] = {
    store.get(id) match {
      case Some(game) => f(game)
      case None => NotFound(new java.util.NoSuchElementException(s"Game with ID=$id does not exist"))
    }
  }
}