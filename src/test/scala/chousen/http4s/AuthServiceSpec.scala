package chousen.http4s

import fs2.{Strategy, Task}
import org.http4s.{Entity, EntityEncoder, Request, UrlForm, Uri, Method}
import org.mockito.Mockito._
import org.scalatest.WordSpec
import org.scalatest.mockito.MockitoSugar


class AuthServiceSpec extends WordSpec with MockitoSugar {

  "Auth Service" when {

    implicit val strategy: Strategy = Strategy.sequential

    val happyMock: GoogleAuthentication = mock[GoogleAuthentication]
    val happyService = new AuthService(happyMock)
    val testToken = "testtoken"

    val form: UrlForm = UrlForm.apply(Map("idtoken" -> Seq(testToken)))
    val enc: EntityEncoder[UrlForm] = UrlForm.entityEncoder
    val ent: Entity = enc.toEntity(form).unsafeRun()
    val req: Request = Request(method = Method.POST, uri = Uri.unsafeFromString(s"/tokensignin"),
      body = ent.body)


    "Authenticator returns successfully" should {

      "Return success with cookie" in {

        when(happyMock.authenticateAsync(testToken)).thenReturn(Task(Option(AuthResponse(Some("testId"), Some("2")))))

        val result = happyService.routes.apply(req).unsafeRun()

        assert(result.orNotFound.status.code == 200)
        assert(result.orNotFound.cookies.nonEmpty)
      }
    }

    "Authenticator returns nothing" should {

      "Return failure" in {
        when(happyMock.authenticateAsync(testToken)).thenReturn(Task(None))

        val result = happyService.routes.apply(req).unsafeRun()

        assert(result.orNotFound.status.code == 400)
        assert(result.orNotFound.cookies.isEmpty)
      }
    }
  }

}
