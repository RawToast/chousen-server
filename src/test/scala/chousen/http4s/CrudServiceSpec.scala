package chousen.http4s

import chousen.api.core.{GameAccess, Http4sMappedGameAccess}
import chousen.game.core.RandomGameStateCreator
import chousen.game.dungeon.{DungeonBuilder, SimpleDungeonBuilder}
import chousen.game.status.StatusCalculator
import fs2.Task
import org.http4s._
import org.scalatest.WordSpec

class CrudServiceSpec extends WordSpec {

  "CrudService" when {

    val dungeonBuilder: DungeonBuilder = new SimpleDungeonBuilder()
    val gameCreator = new RandomGameStateCreator(dungeonBuilder)

    val bobby = gameCreator.create("Bobby")
    val gameMap = Map(bobby.uuid -> bobby)
    val gameAccess: GameAccess[Task, Response] = new Http4sMappedGameAccess(gameMap)

    val sc = new StatusCalculator
    val service = new CrudService(gameAccess, gameCreator, sc)


    "Creating a game" should {

      val callService: (Request) => Task[MaybeResponse] = service.routes.apply(_: Request)
      val req: Request = Request(method = Method.POST, uri = Uri.unsafeFromString("/game/david/start"))
      val task: Task[MaybeResponse] = callService(req)

      lazy val result: Response = task.unsafeRun().orNotFound

      "Return successfully" in {
        assert(result.status.responseClass.isSuccess)
      }

      "Return with a status of Created" in {
        assert(result.status.code == 201)
      }

    }

    "Creating a game with a choice" should {

      val callService: (Request) => Task[MaybeResponse] = service.routes.apply(_: Request)
      val req: Request = Request(method = Method.POST, uri = Uri.unsafeFromString("/game/david/start/1"))
      val task: Task[MaybeResponse] = callService(req)

      lazy val result: Response = task.unsafeRun().orNotFound

      "Return successfully" in {
        assert(result.status.responseClass.isSuccess)
      }

      "Return with a status of Created" in {
        assert(result.status.code == 201)
      }

    }


    "Loading a game that does not exist" should {

      val callService: (Request) => Task[MaybeResponse] = service.routes.apply(_: Request)
      val req: Request = Request(method = Method.GET, uri =
        Uri.unsafeFromString("/game/33673169-266e-417e-a78d-55ba0e2b493c"))
      val task: Task[MaybeResponse] = callService(req)

      lazy val result: Response = task.unsafeRun().orNotFound

      "Return a 404" in {
        assert(!result.status.responseClass.isSuccess)
        assert(result.status.code == 404)
      }
    }

    "Loading a game that exists" should {

      val callService: (Request) => Task[MaybeResponse] = service.routes.apply(_: Request)
      val req: Request = Request(method = Method.GET, uri =
        Uri.unsafeFromString(s"/game/${bobby.uuid}"))
      val task: Task[MaybeResponse] = callService(req)

      lazy val result: Response = task.unsafeRun().orNotFound

      "Return a 200 if the game does not exist" in {
        assert(result.status.responseClass.isSuccess)
        assert(result.status.code == 200)
      }
    }

  }
}
