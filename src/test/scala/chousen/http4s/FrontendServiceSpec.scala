package chousen.http4s

import chousen.api.core.{GameAccess, Http4sMappedGameAccess}
import chousen.game.core.RandomGameStateCreator
import chousen.game.dungeon.{DungeonBuilder, SimpleDungeonBuilder}
import chousen.game.status.StatusCalculator
import fs2.Task
import org.http4s._
import org.scalatest.WordSpec

class FrontendServiceSpec extends WordSpec {

  "FrontendService" when {

    val dungeonBuilder: DungeonBuilder = new SimpleDungeonBuilder()
    val gameCreator = new RandomGameStateCreator(dungeonBuilder)

    val bobby = gameCreator.create("Bobby")
    val gameMap = Map(bobby.uuid -> bobby)
    val gameAccess: GameAccess[Task, Response] = new Http4sMappedGameAccess(gameMap)

    val sc = new StatusCalculator
    val service = new FrontendService(gameAccess, sc)


    "Fetching root" should {

      val callService: (Request) => Task[MaybeResponse] = service.routes.apply(_: Request)
      val req: Request = Request(method = Method.GET, uri = Uri.unsafeFromString("/"))
      val task: Task[MaybeResponse] = callService(req)

      lazy val result: Response = task.unsafeRun().orNotFound

      "Return successfully" in {
        assert(result.status.responseClass.isSuccess)
      }

      "Return with a status of Ok" in {
        assert(result.status.code == 200)
      }

    }

    "Fetching with a game ID" should {

      val callService: (Request) => Task[MaybeResponse] = service.routes.apply(_: Request)
      val req: Request = Request(method = Method.GET, uri = Uri.unsafeFromString(s"/${bobby.uuid}"))
      val task: Task[MaybeResponse] = callService(req)

      lazy val result: Response = task.unsafeRun().orNotFound

      "Return successfully" in {
        assert(result.status.responseClass.isSuccess)
      }

      "Return a status of Ok" in {
        assert(result.status.code == 200)
      }

    }

  }
}
