import java.util.UUID

import chousen.core.{BasicGameManager, Game}
import chousen.data.GameResponse
import chousen.data.Implicits._
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

    Created(GameResponse(game.id, game.player, game.deckManager, game.quest, game.messages))
  }


  val load: Endpoint[GameResponse] = get("game" :: uuid) { id: UUID =>
      store.get(id) match {
        case Some(game) => Ok(GameResponse(game.id, game.player, game.deckManager, game.quest, game.messages))
        case None => NotFound(new java.util.NoSuchElementException(s"Game with ID=$id does not exist"))
      }
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
