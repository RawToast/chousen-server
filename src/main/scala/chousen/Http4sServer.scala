package chousen

import chousen.api.core.{GameAccess, MongoDatastore, MongoGameAccess}
import chousen.api.data.GameState
import chousen.game.core.{GameManager, GameStateManager, RandomGameStateCreator}
import chousen.game.dungeon.{DungeonBuilder, SimpleDungeonBuilder}
import chousen.http4s.{CrudService, FrontendService, InputService}
import fs2.Task
import org.http4s.Response
import org.http4s.util.StreamApp

object Http4sServer extends StreamApp {

  import java.util.concurrent.Executors

  import org.http4s.server.blaze.BlazeBuilder

  override def stream(args: List[String]) = {
    import cats.implicits._
    val port = Option(System.getProperty("http.port")).getOrElse("8080").toInt
    val host = Option(System.getProperty("http.host")).getOrElse("0.0.0.0")

    lazy val mongo = new MongoDatastore(
      "mongodb://chousen:chousen@ds123080.mlab.com:23080/?authSource=heroku_rm14s281&authMechanism=SCRAM-SHA-1",
      "heroku_rm14s281",
      "chousen")

    val gameAccess: GameAccess[Task, Response] = new MongoGameAccess(mongo)

    val dungeonBuilder: DungeonBuilder = new SimpleDungeonBuilder()

    val gameCreator = new RandomGameStateCreator(dungeonBuilder)
    val gameStateManager: GameManager[GameState] = new GameStateManager()


    val crudService = new CrudService(gameAccess, gameCreator)
    val frontendService = new FrontendService(gameAccess)
    val inputService = new InputService(gameAccess, gameStateManager)


    // Unconfigured, will bind to 8080
    BlazeBuilder.bindHttp(port, host)
      .withServiceExecutor(Executors.newCachedThreadPool())
      .mountService(crudService.routes |+| frontendService.routes |+| inputService.routes, "/")
      .serve
  }
}
