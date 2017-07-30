package chousen.http4s

import java.util.UUID

import chousen.api.core.GameAccess
import chousen.game.status.StatusCalculator
import chousen.html
import fs2.Task
import org.http4s.{HttpService, Response}
import org.http4s.dsl._
import play.twirl.api.Html
import org.http4s.twirl._

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
