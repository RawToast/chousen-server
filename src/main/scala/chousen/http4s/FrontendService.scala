package chousen.http4s

import java.util.UUID

import chousen.api.core.GameAccess
import chousen.game.status.StatusCalculator
import chousen.html
import fs2.Task
import org.http4s.dsl.{->, /, GET, Ok, OkSyntax, Root}
import org.http4s.{HttpService, Response}
import play.twirl.api.Html


class FrontendService(apiKey: String, pbga: GameAccess[Task, Response], sc: StatusCalculator) extends HtmlService with ChousenCookie {

  val routes: HttpService = HttpService {

    case GET -> Root =>
      val authPage: Html = chousen.ui.html.auth(apiKey)
      Ok(authPage)

    case req@GET -> Root / "chousen" =>

      val optToken = req.requestToken

      println(s"Received token: $optToken")

      val index: Html = chousen.ui.html.index()
      Ok(index)

    case req@GET -> Root / id =>

      val optToken = req.requestToken

      println(s"Received token: $optToken")

      val uuid = UUID.fromString(id)

      pbga.withGame(uuid, optToken) { game =>
        Ok(html.game.apply(game.copy(player = sc.calculate(game.player))))
      }
  }
}

