//package chousen
//
//import java.util.UUID
//
//import chousen.api.core.MappedGameAccess
//import chousen.api.data.{AttackRequest, GameState}
//import chousen.game.core.GameStateManager
//import com.twitter.finagle.http.{Request, Response}
//import com.twitter.finagle.param.Stats
//import com.twitter.finagle.{Http, Service}
//import com.twitter.io.Buf
//import com.twitter.server.TwitterServer
//import com.twitter.util.Await
//import io.circe.generic.auto._
//import io.finch.{Ok, _}
//import io.finch.circe._
//
//object ChousenServer extends TwitterServer with MappedGameAccess {
//
//  val init: Endpoint[GameState] = post("game" :: string) { playerName: String =>
//    val game: GameState = GameStateManager.create(playerName)
//
//    store = store + (game.id -> game)
//
//    Created(game)
//  }
//
//
//  val load: Endpoint[GameState] = get("game" :: uuid) { id: UUID =>
//    withGame(id) { g => Ok(g) }
//  }
//
//
//  val start: Endpoint[GameState] = post("game" :: "start" :: uuid) { id: UUID =>
//    withGame(id) { g =>
//      val startedGame = GameStateManager.start(g)
//      Ok(startedGame)
//    }
//  }
//
//
//  val attack: Endpoint[GameState] = post("game" :: uuid :: "attack" :: jsonBody[AttackRequest]) { (id: UUID, ar: AttackRequest) =>
//    withGame(id) { g: GameState =>
//      Ok(GameStateManager.takeCommand(ar, g))
//    }
//  }
//
//  val webUiBeta: Endpoint[Response] = get(/) {
//    val content = html.index().toString()
//    val res = Response()
//    res.content = Buf.Utf8(content)
//    res.contentType = "text/html"
//    res
//  }
//
//  val api: Service[Request, Response] = (init :+: load :+: start :+: attack :+: webUiBeta).toServiceAs[Application.Json]
//  val port: String = Option(System.getProperty("http.port")).getOrElse("8080")
//
//
//
//
//
//  def main(): Unit = {
//    args
//    val server = Http.server
//      .configured(Stats(statsReceiver))
//      .serve(s":$port", api)
//
//    onExit {
//      val _ = server.close()
//    }
//
//    val _ = Await.ready(adminHttpServer)
//  }
//}
