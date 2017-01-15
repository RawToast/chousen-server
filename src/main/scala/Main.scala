import chousen.core.BasicGameManager
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

  val init: Endpoint[GameResponse] = get("init" :: string) { playerName: String =>
    val game = BasicGameManager.create(playerName)
    Ok(GameResponse(game.id, game.player, game.deckManager, game.quest, game.messages))
  }

  val api: Service[Request, Response] = init.toServiceAs[Application.Json]
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
