package chousen.http4s

import com.google.api.client.googleapis.auth.oauth2.GoogleIdToken.Payload
import com.google.api.client.googleapis.auth.oauth2.{GoogleIdToken, GoogleIdTokenVerifier}
import org.mockito.Mockito._
import org.scalatest.WordSpec
import org.scalatest.mockito.MockitoSugar


class GoogleAuthenticationSpec extends WordSpec with MockitoSugar {

  "GoogleAuthentication" when {

   // implicit val strategy: Strategy = Strategy.sequential

    val mockVerifier = mock[GoogleIdTokenVerifier]
    val mockToken = mock[GoogleIdToken]
    val mockPayload = mock[Payload]
    val googleAuthentication: GoogleAuthentication = new GoogleAuthentication(mockVerifier)

    val testToken = "testtoken"

    "The verifier returns successfully" should {

      "Return a valid Auth Response" in {
        when(mockVerifier.verify(testToken)).thenReturn(mockToken)
        when(mockToken.getPayload()).thenReturn(mockPayload)

        lazy val result = googleAuthentication.authenticate(testToken)

        assert(result.nonEmpty)
      }
    }

    "Authenticator returns nothing" should {

      "Return failure" in {
        when(mockVerifier.verify(testToken)).thenReturn(null)

        lazy val result = googleAuthentication.authenticate(testToken)

        assert(result.isEmpty)
      }
    }
  }
}
