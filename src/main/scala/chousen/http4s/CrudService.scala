package chousen.http4s

import java.util.UUID

import chousen.api.core.GameAccess
import chousen.game.core.GameStateCreation
import chousen.game.status.StatusCalculator
import fs2.Task
import io.circe.Json
import io.circe.generic.auto._
import io.circe.syntax._
import org.http4s.circe._
import org.http4s.dsl.{->, /, Created, CreatedSyntax, GET, IntVar, Ok, OkSyntax, POST, QueryParamDecoderMatcher, Root}
import org.http4s.{HttpService, Response}

class CrudService(pbga: GameAccess[Task, Response], creator: GameStateCreation, sc: StatusCalculator) extends ChousenCookie {
  object NameMatcher extends QueryParamDecoderMatcher[String]("name")

  val routes: HttpService = HttpService {

    // load
    case req@GET -> Root / "game" / id =>

      val optToken = req.requestToken

      println(s"Received token: $optToken")

      val uuid = UUID.fromString(id)

      pbga.withGame(uuid, optToken) { game =>
        val ng = game.copy(player = sc.calculate(game.player))
        Ok(ng.asJson)
      }

    //  create
    case req@POST -> Root / "game" / playerName / "start"  => // used
      val startedGame = creator.createAndStart(playerName)

      val optToken = req.requestToken

      println(s"Received token: $optToken")

      for {
        game <- pbga.storeGame(startedGame, optToken)
        asJson: Json = game.asJson
        result <- Created(asJson)
      } yield result

    //  create
    case req@POST -> Root / "game" / playerName / "start" / IntVar(choice) => // used
      val startedGame = creator.createAndStart(playerName, choice)
      val optToken = req.requestToken

      println(s"Received token: $optToken")

      for {
        game <- pbga.storeGame(startedGame, optToken)
        asJson: Json = game.asJson
        result <- Created(asJson)
      } yield result
  }

}

