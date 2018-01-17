package chousen.http4s

import cats.data.OptionT
import com.google.api.client.googleapis.auth.oauth2.{GoogleIdToken, GoogleIdTokenVerifier}
import fs2.Task
import fs2.interop.cats._
import io.circe.Json
import io.circe.generic.auto._
import io.circe.syntax._
import org.http4s.{Header, HttpService, Response, UrlForm}
import org.http4s.circe.jsonEncoder
import org.http4s.dsl.{->, /, BadRequest, BadRequestSyntax, Ok, OkSyntax, POST, Root, _}

class AuthService(googleAuthentication: GoogleAuthentication) {

  val routes = HttpService {
    case request@POST -> Root / "tokensignin" =>

      val reqform: Task[UrlForm] = request.as[UrlForm]

      val result: OptionT[Task, Response] = for {
        tokenString: String <- OptionT(reqform.map(form => form.getFirst("idtoken")))
        authResponse: AuthResponse <- OptionT(googleAuthentication.authenticateAsync(tokenString))

        jsonResp: Json = authResponse.asJson
        response: Response <- OptionT.liftF(Ok(jsonResp).addCookie("chousen", authResponse.userId.getOrElse(tokenString)))
      } yield response.putHeaders(Header("Access-Control-Allow-Origin", "*"))

      result.getOrElseF(BadRequest("Unable to complete Google Auth"))
        .putHeaders(Header("Access-Control-Allow-Origin", "*"))
  }
}

class GoogleAuthentication(verifier: GoogleIdTokenVerifier) {


  def authenticate(idToken: String): Option[AuthResponse] = {
    for {
      token: GoogleIdToken <- Option(verifier.verify(idToken))
      payload: GoogleIdToken.Payload <- Option(token.getPayload)
      _ = payload.getEmailVerified
      userId = payload.getSubject
    } yield AuthResponse.create(userId, null)
  }

  def authenticateAsync(idToken: String): Task[Option[AuthResponse]] =
    Task.delay(authenticate(idToken))
}

case class AuthResponse(userId: Option[String], name: Option[String])

object AuthResponse{
  def create(userId: String, name: String) = AuthResponse(Option(userId), Option(name))
}
