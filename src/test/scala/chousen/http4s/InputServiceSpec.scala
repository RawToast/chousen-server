package chousen.http4s

import chousen.api.core.{GameAccess, Http4sMappedGameAccess}
import chousen.api.data.{AttackRequest, GameState}
import chousen.game.actions.DamageCalculator
import chousen.game.core.{GameManager, GameStateManager, RandomGameStateCreator}
import chousen.game.dungeon.{DungeonBuilder, SimpleDungeonBuilder}
import chousen.game.status.StatusCalculator
import fs2.Task
import io.circe.generic.auto._
import org.http4s._
import org.http4s.circe._
import org.scalatest.WordSpec

class InputServiceSpec extends WordSpec {

  "InputServiceSpec" when {

    val dungeonBuilder: DungeonBuilder = new SimpleDungeonBuilder()
    val gameCreator = new RandomGameStateCreator(dungeonBuilder)

    val bobby = gameCreator.create("Bobby")
    val gameMap = Map(bobby.uuid -> bobby)
    val gameAccess: GameAccess[Task, Response] = new Http4sMappedGameAccess(gameMap)

    val statusCalculator = new StatusCalculator
    val damageCalculator = new DamageCalculator(statusCalculator)
    val gameStateManager: GameManager[GameState] = new GameStateManager(damageCalculator)

    val service = new InputService(gameAccess, gameStateManager, statusCalculator)


    "Handling a Basic Attack request" should {

      val attack = AttackRequest(bobby.dungeon.currentEncounter.enemies.head.id)
      implicit val enc: EntityEncoder[AttackRequest] = jsonEncoderOf[AttackRequest]

      val ent: Entity = enc.toEntity(attack).unsafeRun()
      val callService: (Request) => Task[MaybeResponse] = service.routes.apply(_: Request)
      val req: Request = Request(method = Method.POST, uri = Uri.unsafeFromString(s"/game/${bobby.uuid}/attack"),
        body = ent.body)
      val task: Task[MaybeResponse] = callService(req)

      lazy val result: Response = task.unsafeRun().orNotFound

      "Return successfully" in {
        assert(result.status.responseClass.isSuccess)
      }

      "Return with a status of Ok" in {
        assert(result.status.code == 200)
      }

    }
  }
}