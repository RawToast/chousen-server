package chousen.http4s

import java.util.UUID

import chousen.api.core.GameAccess
import chousen.api.data.GameState
import chousen.game.core.GameStateManager
import fs2.Task
import io.circe.generic.auto._
import io.circe.syntax._
import org.http4s.circe._
import org.http4s.dsl._
import org.http4s.{HttpService, Response}

class CrudService(ga: GameAccess[Task, Response]) {
  object NameMatcher extends QueryParamDecoderMatcher[String]("name")

  val routes: HttpService = HttpService {

    // load
    case GET -> Root / "game" / uuid =>

      val id = UUID.fromString(uuid)

      ga.withGame(id) { game =>
        Ok(game.asJson)
      }

    //  create
    case POST -> Root / "game" / playerName / "start" => // used
      val game: GameState = GameStateManager.create(playerName)
      val startedGame = GameStateManager.start(game)

      for {
        game <- ga.storeGame(startedGame)
        asJson = game.asJson
        result <- Created(asJson)
      } yield result
  }

}
