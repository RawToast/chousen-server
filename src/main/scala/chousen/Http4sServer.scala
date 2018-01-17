package chousen

import java.util.Collections

import chousen.api.core.{GameAccess, PlayerBasedGameAccess}
import chousen.api.data.GameState
import chousen.game.actions.DamageCalculator
import chousen.game.core.{GameManager, GameStateManager, RandomGameStateCreator}
import chousen.game.dungeon.{DungeonBuilder, SimpleDungeonBuilder}
import chousen.game.status.{PostTurnStatusCalculator, StatusCalculator}
import chousen.http4s._
import com.google.api.client.googleapis.auth.oauth2.GoogleIdTokenVerifier
import com.google.api.client.googleapis.javanet.GoogleNetHttpTransport
import com.google.api.client.json.jackson2.JacksonFactory
import fs2.Task
import org.http4s.Response
import org.http4s.util.StreamApp

object Http4sServer extends StreamApp {

  import org.http4s.server.blaze.BlazeBuilder

  def buildServer: BlazeBuilder = {
    import cats.implicits.catsSyntaxSemigroup

    val port = Option(System.getProperty("http.port")).getOrElse("8080").toInt
    val host = Option(System.getProperty("http.host")).getOrElse("0.0.0.0")

    // Mongo can be used for persistence
    //    lazy val mongo = new MongoDatastore(

    val dungeonBuilder: DungeonBuilder = new SimpleDungeonBuilder()

    val gameCreator = new RandomGameStateCreator(dungeonBuilder)
    val statusCalculator = new StatusCalculator
    val damageCalculator = new DamageCalculator(statusCalculator)
    val postTurnStatusCalc = new PostTurnStatusCalculator

    val gameStateManager: GameManager[GameState] = new GameStateManager(damageCalculator, postTurnStatusCalc)


    // Note that authentication is not enabled on the frontend
    val apiKey = "<insert_key_here>.apps.googleusercontent.com"
    val googleAuth = new GoogleAuthentication(
      new GoogleIdTokenVerifier.Builder(GoogleNetHttpTransport.newTrustedTransport, JacksonFactory.getDefaultInstance)
        .setAudience(Collections.singletonList(apiKey))
        .build())

    val playerBasedGameAccess: GameAccess[Task, Response] = new PlayerBasedGameAccess()

    val crudService: CrudService = new CrudService(playerBasedGameAccess, gameCreator, statusCalculator)
    val authService = new AuthService(googleAuth)
    val inputService = new InputService(playerBasedGameAccess, gameStateManager, statusCalculator)
    val assetService = new AssetService()
    val executionContext = scala.concurrent.ExecutionContext.global


    BlazeBuilder.bindHttp(port, host)
      .withExecutionContext(executionContext)

      .mountService(crudService.routes |+| inputService.routes |+|
         assetService.routes |+| authService.routes, "/")
  }
  override def stream(args: List[String]) = {
    buildServer.serve
  }
}
