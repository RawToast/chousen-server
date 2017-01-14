//import chousen.core.{BasicGameManager, Game}
//import chousen.data.Implicits._
//import chousen.data._
//import com.twitter.finagle.Http
//import com.twitter.util.Await
//import io.circe.generic.auto._
//import io.finch._
//import io.finch.circe._
//
//
//object Main extends App {
//
//  val init: Endpoint[GameResponse] = get("init" / string) { playerName: String =>
//
//    val game: Game = BasicGameManager.create(playerName)
//
//    Created[GameResponse](GameResponse(
//      game.playerCharacter, game.messages, game.deckManager, game.quest))
//  }
//
//  Await.ready(Http.serve(":8080", init.toServiceAs[Application.Json]))
//}
//
//
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

  val init: Endpoint[GameResponse] = get("init" / string) { playerName: String =>
    val game = BasicGameManager.create(playerName)
    Ok(GameResponse(game.playerCharacter, game.deckManager, game.quest, game.messages))
  }

  val api: Service[Request, Response] = init.toServiceAs[Application.Json]

  def main(): Unit = {
    val server = Http.server
      .configured(Stats(statsReceiver))
      .serve(":8080", api)

    onExit {
      server.close()
    }

    Await.ready(adminHttpServer)
  }
}


