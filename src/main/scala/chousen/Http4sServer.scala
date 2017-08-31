package chousen

import java.util.Collections

import chousen.api.core.{GameAccess, Http4sMappedGameAccess, PlayerBasedGameAccess}
import chousen.api.data.GameState
import chousen.game.actions.DamageCalculator
import chousen.game.core.{GameManager, GameStateManager, RandomGameStateCreator}
import chousen.game.dungeon.{DungeonBuilder, SimpleDungeonBuilder}
import chousen.game.status.{PostTurnStatusCalculator, StatusCalculator}
import chousen.http4s._
import chousen.http4s.auth.{AuthCrudService, AuthFrontendService, AuthInputService}
import com.google.api.client.googleapis.auth.oauth2.GoogleIdTokenVerifier
import com.google.api.client.googleapis.javanet.GoogleNetHttpTransport
import com.google.api.client.json.jackson2.JacksonFactory
import fs2.Task
import org.http4s.Response
import org.http4s.util.StreamApp

object Http4sServer extends StreamApp {

  import java.util.concurrent.Executors
  import org.http4s.server.blaze.BlazeBuilder

  def buildServer: BlazeBuilder = {
    import cats.implicits.catsSyntaxSemigroup

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
    val damageCalculator = new DamageCalculator(statusCalculator)
    val postTurnStatusCalc = new PostTurnStatusCalculator

    val gameStateManager: GameManager[GameState] = new GameStateManager(damageCalculator, postTurnStatusCalc)

    val crudService = new CrudService(gameAccess, gameCreator, statusCalculator)
    val frontendService = new FrontendService(gameAccess, statusCalculator)
    val inputService = new InputService(gameAccess, gameStateManager, statusCalculator)
    val assetService = new AssetService()


    val apiKey = "494987922076-btdj0hccs6u15i90modc5lih6dbiltu6.apps.googleusercontent.com"
    val googleAuth = new GoogleAuthentication(
      new GoogleIdTokenVerifier.Builder(GoogleNetHttpTransport.newTrustedTransport, JacksonFactory.getDefaultInstance)
        .setAudience(Collections.singletonList(apiKey))
        .build())

    val playerBasedGameAccess: GameAccess[Task, Response] = new PlayerBasedGameAccess()

    val authFrontendService = new AuthFrontendService(apiKey, playerBasedGameAccess, statusCalculator)
    val authCrudService = new AuthCrudService(playerBasedGameAccess, gameCreator, statusCalculator)
    val authService = new AuthService(googleAuth)
    val authInputService = new AuthInputService(playerBasedGameAccess, gameStateManager, statusCalculator)


    // Unconfigured, will bind to 8080
    BlazeBuilder.bindHttp(port, host)
      .withServiceExecutor(Executors.newCachedThreadPool())

      .mountService(authFrontendService.routes |+| authService.routes |+|
        authCrudService.routes |+| authInputService.routes |+| assetService.routes, "/new/")

      .mountService(crudService.routes |+| frontendService.routes |+|
        inputService.routes |+| assetService.routes, "/")
  }
  override def stream(args: List[String]) = {
    buildServer.serve
  }
}
