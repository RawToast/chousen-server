package chousen.http4s

import java.util.UUID

import chousen.api.core.GameAccess
import chousen.api.data._
import chousen.game.core.GameManager
import chousen.game.status.StatusCalculator
import fs2.Task
import io.circe.Decoder
import io.circe.generic.auto._
import io.circe.syntax._
import org.http4s.circe._
import org.http4s.dsl.{->, /, NotFound, NotFoundSyntax, Ok, OkSyntax, POST, Root}
import org.http4s.{HttpService, Request, Response}

class InputService(ga: GameAccess[Task, Response], gsm: GameManager[GameState], sc: StatusCalculator) extends ChousenCookie {

  private def getIds(uuid: String, cardUuid: String) =
    (UUID.fromString(uuid), UUID.fromString(cardUuid))

  val routes: HttpService = {
    import io.circe.generic.extras.semiauto._

    HttpService {
      // Attack
      case req@POST -> Root / "game" / uuid / "attack" =>
        val id = UUID.fromString(uuid)
        val optToken = req.requestToken

        ga.withGame(id, optToken) { g =>
          basicRequest[AttackRequest](req, g)(gsm.takeCommand)
        }

      case req@POST -> Root / "game" / uuid / "block" =>
        val id = UUID.fromString(uuid)

        ga.withGame(id) { g =>
          basicRequest[BlockRequest](req, g)(gsm.takeCommand)
        }

      case req@POST -> Root / "game" / uuid / "single" / cardUuid =>
        implicit val enumDecoder: Decoder[SingleTargetAction] = deriveEnumerationDecoder[SingleTargetAction]

        val (id, cardId) = getIds(uuid, cardUuid)
        val optToken = req.requestToken

        ga.withGame(id, optToken) { g =>
          cardRequest[SingleTargetActionRequest](req, g, cardId)(gsm.useCard)
        }

      case req@POST -> Root / "game" / uuid / "self" / cardUuid =>
        implicit val enumDecoder: Decoder[SelfAction] = deriveEnumerationDecoder[SelfAction]

        val (id, cardId) = getIds(uuid, cardUuid)

        val optToken = req.requestToken

        ga.withGame(id, optToken) { g =>
          cardRequest[SelfInflictingActionRequest](req, g, cardId)(gsm.useCard)
        }

      case req@POST -> Root / "game" / uuid / "card" / cardUuid =>
        implicit val enumDecoder: Decoder[CardAction] = deriveEnumerationDecoder[CardAction]

        val (id, cardId) = getIds(uuid, cardUuid)

        val optToken = req.requestToken

        ga.withGame(id, optToken){ g =>
          cardRequest[CardActionRequest](req, g, cardId)(gsm.useCard)
        }

      case req@POST -> Root / "game" / uuid / "multi" / cardUuid =>
        implicit val enumDecoder: Decoder[MultiAction] = deriveEnumerationDecoder[MultiAction]

        val (id, cardId) = getIds(uuid, cardUuid)

        val optToken = req.requestToken

        ga.withGame(id, optToken){ g =>
          cardRequest[MultiTargetActionRequest](req, g, cardId)(gsm.useCard)
        }

      case req@POST -> Root / "game" / uuid / "camp" / cardUuid =>
        implicit val enumDecoder: Decoder[CampFireAction] = deriveEnumerationDecoder[CampFireAction]

        val (id, cardId) = getIds(uuid, cardUuid)

        val optToken = req.requestToken

        ga.withGame(id, optToken) { g =>
          passiveRequest[CampfireActionRequest](req, g, cardId)(gsm.useCard)
        }

      case req@POST -> Root / "game" / uuid / "equip" / cardUuid =>
        implicit val enumDecoder: Decoder[EquipAction] = deriveEnumerationDecoder[EquipAction]

        val (id, cardId) = getIds(uuid, cardUuid)

        val optToken = req.requestToken

        ga.withGame(id, optToken) { g =>
          cardRequest[EquipmentActionRequest](req, g, cardId)(gsm.useCard)
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
        _ <- ga.storeGame(ng, req.requestToken)
        game = ng.copy(player = sc.calculate(ng.player))
        res <- Ok.apply(game.asJson)
      } yield res
      case None => NotFound(g.asJson)
    }
  }

  private def passiveRequest[T <: CommandRequest](req: Request, g: GameState, cardId: UUID)
                                                 (f: (Card, CommandRequest, GameState) => GameState)
                                                 (implicit decoder: Decoder[T]): Task[Response] = {
    g.cards.passive.find(_.id == cardId) match {
      case Some(card) => for {
        ar <- req.as(jsonOf[T])
        ng = f(card, ar, g)
        _ <- ga.storeGame(ng, req.requestToken)
        game = ng.copy(player = sc.calculate(ng.player))
        res <- Ok.apply(game.asJson)
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
      _ <- ga.storeGame(ng, req.requestToken)
      game = ng.copy(player = sc.calculate(ng.player))
      res <- Ok.apply(game.asJson)
    } yield res
  }

}
