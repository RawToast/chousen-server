package chousen.http4s

import java.util.UUID

import chousen.api.core.GameAccess
import chousen.game.status.StatusCalculator
import fs2.Task
import org.http4s.dsl.{->, /, GET, Ok, OkSyntax, Root}
import org.http4s.{HttpService, Response}
import play.twirl.api.Html


class FrontendService(apiKey: String, pbga: GameAccess[Task, Response], sc: StatusCalculator) extends HtmlService with ChousenCookie {

  val routes: HttpService = HttpService {

    case GET -> Root =>
      val authPage: Html = chousen.ui.html.index(apiKey)
      Ok(authPage)

    case req@GET -> Root / id =>

      val optToken = req.requestToken

      val uuid = UUID.fromString(id)

      pbga.withGame(uuid, optToken) { game =>
        Ok(chousen.ui.html.game.apply(game.copy(player = sc.calculate(game.player))))
      }
  }
}

