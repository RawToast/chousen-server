package chousen.api.core

import java.util.UUID

import chousen.api.data._
import fs2.Task
//import io.finch.Output
import org.http4s.Response

trait GameAccess[T] {
  def withGame(id: UUID)(f: GameState => T): T
}

//trait MappedGameAccess extends GameAccess[Output[GameState]] {
//  var store = Map.empty[UUID, GameState]
//
//  def withGame(id: UUID)(f: GameState => Output[GameState]): Output[GameState] = {
//    store.get(id) match {
//      case Some(game) => f(game)
//      case None => io.finch.NotFound(
//        new java.util.NoSuchElementException(s"Game with ID=$id does not exist"))
//    }
//  }
//}

trait Http4sMappedGameAccess extends GameAccess[Task[Response]] {
  import io.circe.generic.auto._
  import io.circe.syntax._
  import org.http4s.circe._
  import org.http4s.dsl._
  var store = Map.empty[UUID, GameState]

  store = store + (GameStateGenerator.uuid -> GameStateGenerator.staticGameState)

  def withGame(id: UUID)(f: GameState => Task[Response]): Task[Response] = {

    case class Error(msg: String)

    store.get(id) match {
      case Some(game) => f(game)
      case None => NotFound(Error(s"Game with ID=$id does not exist").asJson)
    }
  }
}