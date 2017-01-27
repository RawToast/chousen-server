import java.util.UUID

import api.data.GameResponse
import chousen.core.{BasicGameManager, Game, PlayerAttack}
import com.twitter.finagle.http.{Request, Response}
import com.twitter.finagle.param.Stats
import com.twitter.finagle.{Http, Service}
import com.twitter.server.TwitterServer
import com.twitter.util.Await
import io.circe.generic.auto._
import io.finch._
import io.finch.circe._


object Main extends TwitterServer with MappedGameAccess {

  val init: Endpoint[GameResponse] = post("game" :: string) { playerName: String =>
    val game = BasicGameManager.create(playerName)

    store = store + (game.id -> game)

    Created(Game.toResponse(game))
  }


  val load: Endpoint[GameResponse] = get("game" :: uuid) { id: UUID =>
    withGame(id){g => Ok(Game.toResponse(g))}
  }

  val start: Endpoint[GameResponse] = post("game" :: "start" :: uuid) { id: UUID =>
    withGame(id) { g =>
      val startedGame = BasicGameManager.start(g)
      Ok(Game.toResponse(startedGame))
    }
  }

  val attack: Endpoint[GameResponse] = post("game" :: uuid :: jsonBody[AttackRequest]) { a:(UUID, AttackRequest) =>
    withGame(a._1) { g: Game => BasicGameManager.takeCommand(???, g)}
  }

  sealed trait CommandRequest {
    val gameId: UUID
  }

  case class AttackRequest(gameId: UUID, targetId: UUID) extends CommandRequest

  case class SingleTargetActionRequest(gameId: UUID, targetId: UUID, actionId: Int) extends CommandRequest

  case class MultiTargetActionRequest(gameId: UUID, targetId: Set[UUID], actionId: Int) extends CommandRequest

  val api: Service[Request, Response] = (init :+: load :+: start :+: attack).toServiceAs[Application.Json]
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

trait MappedGameAccess extends GameAccess {
  var store = Map.empty[UUID, Game]

  def withGame(id: UUID)(f: Game => Output[GameResponse]): Output[GameResponse] = {
    store.get(id) match {
      case Some(game) => f(game)
      case None => NotFound(new java.util.NoSuchElementException(s"Game with ID=$id does not exist"))
    }
  }
}

trait GameAccess {
  def withGame(id: UUID)(f: Game => Output[GameResponse]): Output[GameResponse]
}
