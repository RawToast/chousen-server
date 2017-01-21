import java.util.UUID

import chousen.core.{BasicGameManager, Game}
import chousen.data.GameResponse
import com.twitter.finagle.http.{Request, Response}
import com.twitter.finagle.param.Stats
import com.twitter.finagle.{Http, Service}
import com.twitter.server.TwitterServer
import com.twitter.util.Await
import io.circe.generic.auto._
import io.finch._
import io.finch.circe._


object Main extends TwitterServer {

  var store = Map.empty[UUID, Game]


  val init: Endpoint[GameResponse] = post("game" :: string) { playerName: String =>
    val game = BasicGameManager.create(playerName)

    store = store + (game.id -> game)

    Created(Game.toResponse(game))
  }


  val load: Endpoint[GameResponse] = get("game" :: uuid) { id: UUID =>
      store.get(id) match {
        case Some(game) => Ok(Game.toResponse(game))
        case None => NotFound(new java.util.NoSuchElementException(s"Game with ID=$id does not exist"))
      }
    }

  val start: Endpoint[GameResponse] = get("game" :: "start" :: uuid) { id: UUID =>
    val game = store.get(id)

    val g = game.get
    val startedGame = BasicGameManager.start(g)

    Ok(Game.toResponse(startedGame))
  }

  val api: Service[Request, Response] = (init :+: load).toServiceAs[Application.Json]
  val port: String = Option(System.getProperty("http.port")).getOrElse("8080")

  def main(): Unit = {
    args
    val server = Http.server
      .configured(Stats(statsReceiver))
      .serve(s":$port", api)

    onExit {
      val _ = server.close()
    }

    val _ = Await.ready(adminHttpServer)
  }
}
