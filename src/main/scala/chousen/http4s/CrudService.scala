package chousen.http4s

import java.util.UUID

import chousen.api.core.GameAccess
import chousen.game.core.GameStateCreation
import chousen.game.status.StatusCalculator
import fs2.Task
import io.circe.generic.auto._
import io.circe.syntax._
import io.circe.{Json, Printer}
import org.http4s.{EntityEncoder, Header}
//import org.http4s.dsl.{->, /, Created, CreatedSyntax, GET, IntVar, Ok, OkSyntax, POST, QueryParamDecoderMatcher, Root}
import org.http4s.circe.jsonEncoderWithPrinter
import org.http4s.dsl._
import org.http4s.{HttpService, Response}

class CrudService(pbga: GameAccess[Task, Response], creator: GameStateCreation, sc: StatusCalculator) extends ChousenCookie {

  object NameMatcher extends QueryParamDecoderMatcher[String]("name")

  val routes: HttpService = {

    implicit def jsonEnc: EntityEncoder[Json] = jsonEncoderWithPrinter(Printer.noSpaces.copy(dropNullKeys = true))

    HttpService {

      // load
      case req@GET -> Root / "game" / id =>

        val optToken = req.requestToken

        val uuid = UUID.fromString(id)

        pbga.withGame(uuid, optToken) { game =>
          val ng = game.copy(player = sc.calculate(game.player))
          val resp = ng.asResponse
          //Access-Control-Allow-Origin: *
          Ok(resp.asJson).putHeaders(Header("Access-Control-Allow-Origin", "*"))
        }

      //  create
      case req@POST -> Root / "game" / playerName / "start" => // used
        val startedGame = creator.createAndStart(playerName)

        val optToken = req.requestToken

        for {
          game <- pbga.storeGame(startedGame, optToken)
          resp = game.asResponse
          asJson: Json = resp.asJson
          result <- Created(asJson)
        } yield result.putHeaders(Header("Access-Control-Allow-Origin", "*"))

      //  create
      case req@POST -> Root / "game" / playerName / "start" / IntVar(choice) => // used
        val startedGame = creator.createAndStart(playerName, choice)
        val optToken = req.requestToken

        for {
          game <- pbga.storeGame(startedGame, optToken)
          resp = game.asResponse
          asJson: Json = resp.asJson
          result <- Created(asJson)
        } yield result.putHeaders(Header("Access-Control-Allow-Origin", "*"))
    }
  }


}

