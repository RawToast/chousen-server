package chousen

import chousen.api.core.{GameAccess, Http4sMappedGameAccess}
import chousen.api.data.GameState
import chousen.game.core.{GameManager, GameStateManager, RandomGameStateCreator}
import chousen.game.dungeon.{DungeonBuilder, SimpleDungeonBuilder}
import chousen.game.status.StatusCalculator
import chousen.http4s.{AssetService, CrudService, FrontendService, InputService}
import fs2.Task
import org.http4s.Response
import org.http4s.util.StreamApp

object Http4sServer extends StreamApp {

  import java.util.concurrent.Executors

  import org.http4s.server.blaze.BlazeBuilder

  def buildServer: BlazeBuilder = {
    import cats.implicits._
    val port = Option(System.getProperty("http.port")).getOrElse("8080").toInt
    val host = Option(System.getProperty("http.host")).getOrElse("0.0.0.0")

    //    lazy val mongo = new MongoDatastore(
    //      "mongodb://chousen:chousen@ds123080.mlab.com:23080/?authSource=heroku_rm14s281&authMechanism=SCRAM-SHA-1",
    //      "heroku_rm14s281",
    //      "chousen")

    val gameAccess: GameAccess[Task, Response] = new Http4sMappedGameAccess()

    val dungeonBuilder: DungeonBuilder = new SimpleDungeonBuilder()

    val gameCreator = new RandomGameStateCreator(dungeonBuilder)
    val statusCalculator = new StatusCalculator
    val gameStateManager: GameManager[GameState] = new GameStateManager(statusCalculator)


    val crudService = new CrudService(gameAccess, gameCreator, statusCalculator)
    val frontendService = new FrontendService(gameAccess, statusCalculator)
    val inputService = new InputService(gameAccess, gameStateManager, statusCalculator)
    val assetService = new AssetService()


    // Unconfigured, will bind to 8080
    BlazeBuilder.bindHttp(port, host)
      .withServiceExecutor(Executors.newCachedThreadPool())
      .mountService(crudService.routes |+| frontendService.routes |+|
        inputService.routes |+| assetService.routes, "/")
  }
  override def stream(args: List[String]) = {
    buildServer.serve
  }
}
