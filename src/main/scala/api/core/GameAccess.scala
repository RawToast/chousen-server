package api.core

import java.util.UUID

import api.data.GameResponse
import chousen.core.Game
import io.finch.{NotFound, Output}

trait GameAccess {
  def withGame(id: UUID)(f: Game => Output[GameResponse]): Output[GameResponse]
}

trait MappedGameAccess extends GameAccess {
  var store = Map.empty[UUID, Game]

  def withGame(id: UUID)(f: Game => Output[GameResponse]): Output[GameResponse] = {
    store.get(id) match {
      case Some(game) => f(game)
      case None => NotFound(new java.util.NoSuchElementException(s"Game with ID=$id does not exist"))
    }
  }
}