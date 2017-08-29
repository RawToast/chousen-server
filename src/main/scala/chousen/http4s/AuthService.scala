package chousen.http4s

import cats.data.OptionT
import com.google.api.client.googleapis.auth.oauth2.GoogleIdTokenVerifier
import fs2.Task
import fs2.interop.cats._
import io.circe.Json
import io.circe.generic.auto._
import io.circe.syntax._
import org.http4s._
import org.http4s.circe._
import org.http4s.dsl._


class AuthService(googleAuthentication: GoogleAuthentication) {

  val tokenService = HttpService {
    case request@POST -> Root / "tokensignin" =>

      val reqform: Task[UrlForm] = request.as[UrlForm]

      val result: OptionT[Task, Json] = for {
        tokenString: String <- OptionT(reqform.map(form => form.getFirst("idtoken")))
        authResponse: AuthResponse <- OptionT(googleAuthentication.authenticateAsync(tokenString))

        jsonResp: Json = authResponse.asJson
      } yield jsonResp

      result.semiflatMap(r => Ok(r))
        .getOrElseF(BadRequest("Unable to complete Google Auth"))
  }
}

class GoogleAuthentication(verifier: GoogleIdTokenVerifier) {


  def authenticate(idToken: String): Option[AuthResponse] = {

   // val optToken: Option[GoogleIdToken] = Option(verifier.verify(idToken)) // can return null

    for {
      token <- Option(verifier.verify(idToken))
      payload = token.getPayload
      name = payload.get("name").asInstanceOf[String]
      userId = payload.getSubject
    } yield AuthResponse.create(userId, name)
  }

  def authenticateAsync(idToken: String): Task[Option[AuthResponse]] =
    Task.delay(authenticate(idToken))
}

case class AuthResponse(userId: Option[String], name: Option[String])

object AuthResponse{
  def create(userId: String, name: String) = AuthResponse(Option(name), Option(userId))
}
