package chousen.http4s

import java.util.UUID

import chousen.api.core.GameAccess
import chousen.game.status.StatusCalculator
import chousen.html
import fs2.Task
import org.http4s.dsl._
import org.http4s.headers.Cookie
import org.http4s.twirl._
import org.http4s.{HttpService, Response}
import play.twirl.api.Html

class FrontendService(ga: GameAccess[Task, Response], sc: StatusCalculator) {

  val routes: HttpService = HttpService {
    // init
    case GET -> Root =>
      val index: Html = html.index()
      Ok(index)

    case GET -> Root / id =>

      val uuid = UUID.fromString(id)

      ga.withGame(uuid) { game =>
        Ok(html.game.apply(game.copy(player = sc.calculate(game.player))))
      }
  }

}

class NewFrontendService(apiKey: String) {

  val routes: HttpService = HttpService {

    case GET -> Root =>
      val authPage: Html = chousen.ui.html.auth(apiKey)
      Ok(authPage)

    case req@GET -> Root / "chousen" =>

//      val headers = Headers(
//        org.http4s.headers.`Cookie`(org.http4s.Cookie("foo", "bar")),
//        Header("Cookie", org.http4s.Cookie("baz", "quux").toString)
//      )
//      headers.get(org.http4s.headers.Cookie).map(_.values.length) must beSome (2)

      def findChousenCookie(c: Cookie) = c.values.find(_.name == "chousen")

      val optToken: Option[String] = for {
        headers <- req.headers.get(Cookie)
        cookie <- findChousenCookie(headers)
        token: String = cookie.content
      } yield token

      println(s"Received token: $optToken")


      val index: Html = html.index()
      Ok(index)
  }

}
