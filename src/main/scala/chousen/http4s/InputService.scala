package chousen.http4s

import java.util.UUID

import chousen.api.core.GameAccess
import chousen.api.data._
import chousen.game.core.GameStateManager
import fs2.Task
import io.circe.Decoder
import io.circe.generic.auto._
import io.circe.syntax._
import org.http4s.{HttpService, Request, Response}
import org.http4s.circe._
import org.http4s.dsl._

class InputService(ga: GameAccess[Task, Response]) {

  private def getIds(uuid: String, cardUuid: String) =
    (UUID.fromString(uuid), UUID.fromString(cardUuid))

  val routes: HttpService = {
    import io.circe.generic.extras.semiauto._

    HttpService {
      // Attack
      case req@POST -> Root / "game" / uuid / "attack" =>
        val id = UUID.fromString(uuid)

        ga.withGame(id) { g =>
          basicRequest[AttackRequest](req, g)(GameStateManager.takeCommand)
        }


      case req@POST -> Root / "game" / uuid / "single" / cardUuid =>
        implicit val enumDecoder = deriveEnumerationDecoder[SingleTargetAction]

        val (id, cardId) = getIds(uuid, cardUuid)

        ga.withGame(id) { g =>
          cardRequest[SingleTargetActionRequest](req, g, cardId)(GameStateManager.useCard)
        }

      case req@POST -> Root / "game" / uuid / "self" / cardUuid =>
        implicit val enumDecoder = deriveEnumerationDecoder[SelfAction]

        val (id, cardId) = getIds(uuid, cardUuid)

        ga.withGame(id) { g =>
          cardRequest[SelfInflictingActionRequest](req, g, cardId)(GameStateManager.useCard)
        }

      case req@POST -> Root / "game" / uuid / "card" / cardUuid =>
        implicit val enumDecoder = deriveEnumerationDecoder[CardAction]

        val (id, cardId) = getIds(uuid, cardUuid)

        ga.withGame(id) { g =>
          cardRequest[CardActionRequest](req, g, cardId)(GameStateManager.useCard)
        }

      case req@POST -> Root / "game" / uuid / "multi" / cardUuid =>
        implicit val enumDecoder = deriveEnumerationDecoder[MultiAction]

        val (id, cardId) = getIds(uuid, cardUuid)

        ga.withGame(id) { g =>
          cardRequest[MultiTargetActionRequest](req, g, cardId)(GameStateManager.useCard)
        }
    }
  }

  private def cardRequest[T <: CommandRequest](req: Request, g: GameState, cardId: UUID)
                                      (f: (Card, CommandRequest, GameState) => GameState)
                                      (implicit decoder: Decoder[T]): Task[Response] = {
      g.cards.hand.find(_.id == cardId) match {
        case Some(card) => for {
          ar <- req.as(jsonOf[T])
          ng = f(card, ar, g)
          _ <- ga.storeGame(ng)
          res <- Ok.apply(ng.asJson)
        } yield res
        case None => NotFound(g.asJson)
      }
  }

  private def basicRequest[T <: CommandRequest](req: Request, g: GameState)
                                              (f: (CommandRequest, GameState) => GameState)
                                              (implicit decoder: Decoder[T]): Task[Response] = {
      for {
        ar <- req.as(jsonOf[T])
        ng = f(ar, g)
        _ <- ga.storeGame(ng)
        res <- Ok.apply(ng.asJson)
      } yield res
  }

}
