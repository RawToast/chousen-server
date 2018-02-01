package chousen.http4s

import java.util.UUID

import chousen.api.core.GameAccess
import chousen.api.data._
import chousen.game.core.GameManager
import chousen.game.status.StatusCalculator
import fs2.Task
import io.circe.{Decoder, Encoder, Json, Printer}
import io.circe.generic.auto._
import io.circe.syntax._
import org.http4s.circe.{jsonEncoderWithPrinter, jsonOf}
import org.http4s.dsl.{->, /, NotFound, NotFoundSyntax, Ok, OkSyntax, POST, Root, _}
import org.http4s.{EntityEncoder, Header, HttpService, Request, Response}

class InputService(ga: GameAccess[Task, Response], gsm: GameManager[GameState], sc: StatusCalculator) extends ChousenCookie {

  private def getIds(uuid: String, cardUuid: String) =
    (UUID.fromString(uuid), UUID.fromString(cardUuid))
  import io.circe.generic.extras.semiauto._

  implicit def jsonEnc: EntityEncoder[Json] = jsonEncoderWithPrinter(Printer.noSpaces.copy(dropNullKeys = true))
  implicit def statusEncoder: Encoder[StatusEffect] = deriveEnumerationEncoder[StatusEffect]

  implicit val singleEnumDecoder: Decoder[SingleTargetAction] = deriveEnumerationDecoder[SingleTargetAction]

  implicit val selfEnumDecoder: Decoder[SelfAction] = deriveEnumerationDecoder[SelfAction]
  implicit val cardEnumDecoder: Decoder[CardAction] = deriveEnumerationDecoder[CardAction]
  implicit val multiEnumDecoder: Decoder[MultiAction] = deriveEnumerationDecoder[MultiAction]
  implicit val campEnumDecoder: Decoder[CampFireAction] = deriveEnumerationDecoder[CampFireAction]
  implicit val equipEnumDecoder: Decoder[EquipAction] = deriveEnumerationDecoder[EquipAction]

  val routes: HttpService = {

    HttpService {
      // Attacks
      case req@POST -> Root / "game" / uuid / "attack" =>
        val id = UUID.fromString(uuid)

        val optToken = req.requestToken

        ga.withGame(id, optToken) { g =>
          basicRequest[AttackRequest](req, g)(gsm.takeCommand)
        }

      case req@POST -> Root / "game" / uuid / "block" =>
        val id = UUID.fromString(uuid)
        val optToken = req.requestToken

        ga.withGame(id, optToken) { g =>
          basicRequest[BlockRequest](req, g)(gsm.takeCommand)
        }

      case req@POST -> Root / "game" / uuid / "single" / cardUuid =>
        val (id, cardId) = getIds(uuid, cardUuid)
        val optToken = req.requestToken

        ga.withGame(id, optToken) { g =>
          cardRequest[SingleTargetActionRequest](req, g, cardId)(gsm.useCard)
        }

      case req@POST -> Root / "game" / uuid / "self" / cardUuid =>
        val (id, cardId) = getIds(uuid, cardUuid)

        val optToken = req.requestToken

        ga.withGame(id, optToken) { g =>
          cardRequest[SelfInflictingActionRequest](req, g, cardId)(gsm.useCard)
        }

      case req@POST -> Root / "game" / uuid / "card" / cardUuid =>
        val (id, cardId) = getIds(uuid, cardUuid)

        val optToken = req.requestToken

        ga.withGame(id, optToken){ g =>
          cardRequest[CardActionRequest](req, g, cardId)(gsm.useCard)
        }

      case req@POST -> Root / "game" / uuid / "multi" / cardUuid =>
        val (id, cardId) = getIds(uuid, cardUuid)

        val optToken = req.requestToken

        ga.withGame(id, optToken){ g =>
          cardRequest[MultiTargetActionRequest](req, g, cardId)(gsm.useCard)
        }

      case req@POST -> Root / "game" / uuid / "camp" / cardUuid =>
        val (id, cardId) = getIds(uuid, cardUuid)

        val optToken = req.requestToken

        val resp = ga.withGame(id, optToken) { g =>
          passiveRequest[CampfireActionRequest](req, g, cardId)(gsm.useCard)
        }

        resp.map(_.putHeaders(Header("Access-Control-Allow-Origin", "*")))

      case req@POST -> Root / "game" / uuid / "equip" / cardUuid =>
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

    val optCard = g.cards.hand
      .find(_.id == cardId)
      .fold(g.cards.equippedCards.skills.find(_.id == cardId))((c: Card) => Option(c))

    optCard match {
      case Some(card) => for {
        ar <- req.as(jsonOf[T])
        ng = f(card, ar, g)
        _ <- ga.storeGame(ng, req.requestToken)
        game = ng.copy(player = sc.calculate(ng.player))
        resp = game.asResponse(g.messages)
        res <- Ok.apply(resp.asJson)
      } yield res.putHeaders(Header("Access-Control-Allow-Origin", "*"))
      case None => NotFound(g.asJson).map(_.putHeaders(Header("Access-Control-Allow-Origin", "*")))
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
        resp = game.asResponse(g.messages)
        res <- Ok.apply(resp.asJson)
      } yield res.putHeaders(Header("Access-Control-Allow-Origin", "*"))
      case None => NotFound(g.asJson).putHeaders(Header("Access-Control-Allow-Origin", "*"))
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
      resp = game.asResponse(g.messages)
      res <- Ok.apply(resp.asJson).map(_.putHeaders(Header("Access-Control-Allow-Origin", "*")))
    } yield res.putHeaders(Header("Access-Control-Allow-Origin", "*"))
  }

}
