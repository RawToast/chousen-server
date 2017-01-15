package services

import java.util.UUID

import controllers.CreateResponse
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{Matchers, WordSpec}

import scala.concurrent.Future

class GameServiceTest extends WordSpec with Matchers with ScalaFutures {

  "The GameController" when {

    val service: GameService = new DummyGameService
    val testID = UUID.fromString("27a4ac0e-bae3-48fa-84ba-326ab52d071e")

    "Initialising a game" should {

      "Start a new game if requested" in {
        val resultFuture: Future[UUID] = service.initGame("TestUser")

        whenReady(resultFuture) { result =>
          result should be(testID)
        }
      }

      "Continue an existing game if session data exists" in {
        val resultFuture: Future[CreateResponse] = service.fetchGame(testID)

        whenReady(resultFuture) { result =>
          result.id should be(testID)
        }
      }

      "Return an error if an unknown game ID is requested" in {

        val resultFuture: Future[CreateResponse] = service.fetchGame(UUID.randomUUID())

        whenReady(resultFuture.failed) { (result: Throwable) => {
          result.getMessage should be("Bad id")
        }
        }
      }
    }

    "Creating a game" should {


    }
  }
}
