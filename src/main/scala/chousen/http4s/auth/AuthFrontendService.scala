package chousen.http4s.auth

import java.util.UUID

import chousen.api.core.GameAccess
import chousen.api.data._
import chousen.game.core.{GameManager, GameStateCreation}
import chousen.game.status.StatusCalculator
import chousen.html
import chousen.http4s.HtmlService
import fs2.Task
import io.circe.generic.auto._
import io.circe.syntax._
import io.circe.{Decoder, Json}
import org.http4s
import org.http4s.circe._
import org.http4s.dsl.{->, /, Created, CreatedSyntax, GET, IntVar, NotFound, NotFoundSyntax, Ok, OkSyntax, POST, QueryParamDecoderMatcher, Root}
import org.http4s.headers.Cookie
import org.http4s.{HttpService, Request, Response}
import play.twirl.api.Html

trait ChousenCookie {
  import org.http4s.Request
  def findChousenCookie(c: Cookie): Option[http4s.Cookie] = c.values.find(_.name == "chousen")

  implicit class TokenSyntax(req: Request) {

    def requestToken: Option[String] = for {
      headers <- req.headers.get(Cookie)
      cookie <- findChousenCookie(headers)
      token: String = cookie.content
    } yield token
  }
}


class AuthFrontendService(apiKey: String, pbga: GameAccess[Task, Response], sc: StatusCalculator) extends HtmlService with ChousenCookie {

  val routes: HttpService = HttpService {

    case GET -> Root =>
      val authPage: Html = chousen.ui.html.auth(apiKey)
      Ok(authPage)

    case req@GET -> Root / "chousen" =>

      val optToken = req.requestToken

      println(s"Received token: $optToken")

      val index: Html = chousen.ui.html.index()
      Ok(index)

    case req@GET -> Root / id =>

      val optToken = req.requestToken

      println(s"Received token: $optToken")

      val uuid = UUID.fromString(id)

      pbga.withGame(uuid, optToken) { game =>
        Ok(html.game.apply(game.copy(player = sc.calculate(game.player))))
      }
  }
}

class AuthCrudService(pbga: GameAccess[Task, Response], creator: GameStateCreation, sc: StatusCalculator) extends ChousenCookie {
  object NameMatcher extends QueryParamDecoderMatcher[String]("name")

  val routes: HttpService = HttpService {

    // load
    case req@GET -> Root / "game" / id =>

      val optToken = req.requestToken

      println(s"Received token: $optToken")

      val uuid = UUID.fromString(id)

      pbga.withGame(uuid, optToken) { game =>
        val ng = game.copy(player = sc.calculate(game.player))
        Ok(ng.asJson)
      }

    //  create
    case req@POST -> Root / "game" / playerName / "start"  => // used
      val startedGame = creator.createAndStart(playerName)

      val optToken = req.requestToken

      println(s"Received token: $optToken")

      for {
        game <- pbga.storeGame(startedGame, optToken)
        asJson: Json = game.asJson
        result <- Created(asJson)
      } yield result

    //  create
    case req@POST -> Root / "game" / playerName / "start" / IntVar(choice) => // used
      val startedGame = creator.createAndStart(playerName, choice)
      val optToken = req.requestToken

      println(s"Received token: $optToken")

      for {
        game <- pbga.storeGame(startedGame, optToken)
        asJson: Json = game.asJson
        result <- Created(asJson)
      } yield result
  }

}


class AuthInputService(ga: GameAccess[Task, Response], gsm: GameManager[GameState], sc: StatusCalculator) extends ChousenCookie {

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
