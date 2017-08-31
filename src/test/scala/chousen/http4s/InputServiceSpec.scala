package chousen.http4s

import java.util.UUID

import chousen.Optics._
import chousen.api.core.{GameAccess, Http4sMappedGameAccess}
import chousen.api.data._
import chousen.game.actions.DamageCalculator
import chousen.game.core.{GameManager, GameStateManager, RandomGameStateCreator}
import chousen.game.dungeon.{DungeonBuilder, SimpleDungeonBuilder}
import chousen.game.status.{PostTurnStatusCalculator, StatusCalculator}
import fs2.Task
import io.circe.generic.auto._
import io.circe.generic.extras.semiauto.deriveEnumerationEncoder
import org.http4s.{MaybeResponse, Request, Response, Method, Uri, Entity, EntityEncoder}
import org.http4s.circe._
import org.scalatest.WordSpec

class InputServiceSpec extends WordSpec {

  "InputServiceSpec" when {

    val dungeonBuilder: DungeonBuilder = new SimpleDungeonBuilder()
    val gameCreator = new RandomGameStateCreator(dungeonBuilder)

    val chainmailCardId = "12224f23-d5aa-4026-a229-56b881479714"
    val chainmailCard = Card(UUID.fromString(chainmailCardId), "Chainmail",  "test", Chainmail)

    val game = gameCreator.create("Bobby")

    val bobby = HandLens.modify(_ :+ chainmailCard)(game)

    val gameMap = Map(bobby.uuid -> bobby)
    val gameAccess: GameAccess[Task, Response] = new Http4sMappedGameAccess(gameMap)

    val statusCalculator = new StatusCalculator
    val damageCalculator = new DamageCalculator(statusCalculator)
    val postTurnCalc = new PostTurnStatusCalculator

    val gameStateManager: GameManager[GameState] = new GameStateManager(damageCalculator, postTurnCalc)

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

    "Handling a Block request" should {

      val attack = BlockRequest()
      implicit val enc: EntityEncoder[BlockRequest] = jsonEncoderOf[BlockRequest]

      val ent: Entity = enc.toEntity(attack).unsafeRun()
      val callService: (Request) => Task[MaybeResponse] = service.routes.apply(_: Request)
      val req: Request = Request(method = Method.POST, uri = Uri.unsafeFromString(s"/game/${bobby.uuid}/block"),
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

    "Handling an Equipment request" when {
      implicit val enumDecoder = deriveEnumerationEncoder[EquipAction]
      implicit val enc: EntityEncoder[EquipmentActionRequest] = jsonEncoderOf[EquipmentActionRequest]

      "When given an valid equipment request" should {
        val actionRequest = EquipmentActionRequest(UUID.fromString(chainmailCardId), Chainmail)

        val ent: Entity = enc.toEntity(actionRequest).unsafeRun()
        val callService: (Request) => Task[MaybeResponse] = service.routes.apply(_: Request)

        val req: Request = Request(method = Method.POST,
          uri = Uri.unsafeFromString(s"/game/${bobby.uuid}/equip/$chainmailCardId"),
          body = ent.body)
        val task: Task[MaybeResponse] = callService(req)

        lazy val result: Response = task.unsafeRun().orNotFound

        "Return successfully" in {
          assert(result.status.responseClass.isSuccess)
        }

        "Return with a status of Ok (200)" in {
          assert(result.status.code == 200)
        }
      }

      "When given an invalid Card ID" should {
        val altId = UUID.randomUUID()
        val actionRequest = EquipmentActionRequest(altId, Chainmail)

        val ent: Entity = enc.toEntity(actionRequest).unsafeRun()
        val callService: (Request) => Task[MaybeResponse] = service.routes.apply(_: Request)

        val req: Request = Request(method = Method.POST,
          uri = Uri.unsafeFromString(s"/game/${bobby.uuid}/equip/$altId"),
          body = ent.body)
        val task: Task[MaybeResponse] = callService(req)

        lazy val result: Response = task.unsafeRun().orNotFound

        "Return unsuccessfully" in {
          assert(!result.status.responseClass.isSuccess)
        }

        "Return with a status of NotFound (404)" in {
          assert(result.status.code == 404)
        }
      }



    }


  }
}