import java.util.UUID

import api.core.MappedGameAccess
import api.data.{AttackRequest, GameState}
import api.error.TargetNotFoundException
import chousen.core.old.{BasicGameManager, Command, Game, PlayerAttack}
import com.twitter.finagle.http.{Request, Response}
import com.twitter.finagle.param.Stats
import com.twitter.finagle.{Http, Service}
import com.twitter.server.TwitterServer
import com.twitter.util.Await
import io.circe.generic.auto._
import io.finch._
import io.finch.circe._


object Main extends TwitterServer with MappedGameAccess {

  val init: Endpoint[GameState] = post("game" :: string) { playerName: String =>
    val game = BasicGameManager.create(playerName)

    store = store + (game.id -> game)

    Created(Game.toResponse(game))
  }


  val load: Endpoint[GameState] = get("game" :: uuid) { id: UUID =>
    withGame(id) { g => Ok(Game.toResponse(g)) }
  }

  val start: Endpoint[GameState] = post("game" :: "start" :: uuid) { id: UUID =>
    withGame(id) { g =>
      val startedGame = BasicGameManager.start(g)
      Ok(Game.toResponse(startedGame))
    }
  }


  val attack: Endpoint[GameState] = post("game" :: uuid :: "attack" :: jsonBody[AttackRequest]) { (id:UUID, ar:AttackRequest) =>
    withGame(id) { g: Game =>

      val target = Game.currentEnemies.get(g).filter(b => b.id == ar.targetId)

      if (target.nonEmpty) {
        val command = Command(target, PlayerAttack)

        Ok(Game.toResponse(BasicGameManager.takeCommand(command, g)))
      } else BadRequest(TargetNotFoundException.raise(id, Game.currentEnemies.get(g).map(_.id)))
    }
  }

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
