package chousen.api.core

import java.util.UUID

import chousen.api.data._
import fs2.{Strategy, Task}

//import io.finch.Output
import org.http4s.Response

trait GameAccess[T[_], R] {
  def withGame(id: UUID)(f: GameState => T[R]): T[R]

  def storeGame(g: GameState): T[GameState]
}

class MongoGameAccess(mongoDatastore: MongoDatastore) extends GameAccess[Task, Response] {
  override def withGame(id: UUID)(f: GameState => Task[Response]): Task[Response] = for {
    gameState <- mongoDatastore.get(id)
    response <- f(gameState)
  } yield response


  override def storeGame(g: GameState): Task[GameState] =
    mongoDatastore.put(g)
}

class Http4sMappedGameAccess(private var store: Map[UUID, GameState] = Map.empty) extends GameAccess[Task, Response] {
  import io.circe.generic.auto._
  import io.circe.syntax._
  import org.http4s.circe._
  import org.http4s.dsl._

  def withGame(id: UUID)(f: GameState => Task[Response]): Task[Response] = {

    case class Error(msg: String)

    store.get(id) match {
      case Some(game) => f(game)
      case None => NotFound(Error(s"Game with ID=$id does not exist").asJson)
    }
  }

  override def storeGame(g: GameState): Task[GameState] = {
    implicit val executionContext = scala.concurrent.ExecutionContext.Implicits.global

    implicit val strategy =
      Strategy.fromExecutionContext(executionContext)

    store = store + (g.uuid -> g)
    Task(g)
  }
}
