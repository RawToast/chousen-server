package chousen

import java.util.UUID

import chousen.api.core.Http4sMappedGameAccess
import chousen.api.data.{AttackRequest, GameState}
import chousen.game.core.GameStateManager
import org.http4s.util.StreamApp
import play.twirl.api.Html

object Http4sServer extends StreamApp with Http4sMappedGameAccess {

  import java.util.concurrent.Executors

  import org.http4s._
  import org.http4s.dsl._
  import org.http4s.server.blaze.BlazeBuilder
  import org.http4s.twirl._
  import org.http4s.circe._
  import io.circe.generic.auto._
  import io.circe.syntax._



  object NameMatcher extends QueryParamDecoderMatcher[String]("name")


  val frontend: HttpService = HttpService {
    // init
    case GET -> Root =>
      val index: Html = html.index()
      Ok(index)

    case GET -> Root / id =>

      val uuid = UUID.fromString(id)

      withGame(uuid) { game =>
        Created(html.game.apply(game))
      }

  }


  val apiEndpoints: HttpService = HttpService {

    // load
    case GET -> Root / "game" / uuid =>

      val id = UUID.fromString(uuid)

      withGame(id) { game =>
        Ok(game.asJson)
      }

    //  begin
    case POST -> Root / "game" / playerName =>

      val game: GameState = GameStateManager.create(playerName)

      store = store + (game.id -> game)

      Created(game.asJson)

    //  create
    case POST -> Root / "game" / playerName / "start" =>

      val game: GameState = GameStateManager.create(playerName)
      val startedGame = GameStateManager.start(game)

      store = store + (game.id -> startedGame)

      Created(game.asJson)

    // Start (remove?)
    case POST -> Root / "game" / "start" / uuid =>
      val id = UUID.fromString(uuid)

      withGame(id) { game =>
        val startedGame = GameStateManager.start(game)
        Ok(startedGame.asJson)
      }
  }


  val inputEndpoints = HttpService {
    // Attack
    case req@POST -> Root / "game" / uuid / "attack" =>
      val id = UUID.fromString(uuid)

      withGame(id) { g =>
        for {
          ar <- req.as(jsonOf[AttackRequest])
          ng = GameStateManager.takeCommand(ar, g)
          res <- Ok.apply(ng.asJson)
        } yield res
      }
  }

  override def stream(args: List[String]) = {
    import cats.implicits._

    // Unconfigured, will bind to 8080
    BlazeBuilder.bindHttp()
      .withServiceExecutor(Executors.newCachedThreadPool())
      .mountService(apiEndpoints |+| frontend |+| inputEndpoints, "/")
      .serve
  }
}