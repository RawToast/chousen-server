package chousen.http4s

import org.http4s.Request
import org.http4s.headers.Cookie
import org.scalatest.WordSpec

class ChousenCookieSpec extends WordSpec {

  "ChousenCookie" should {

    val chousenCookie = new ChousenCookie {}

    "Return the cookie context if one exists" in {

      val testRequest = Request.apply(org.http4s.dsl.GET).addCookie("chousen", "test")
      val xx: Cookie = Cookie.from(testRequest.headers).get

      val testCookie  = chousenCookie.findChousenCookie(xx)

      assert(testCookie.nonEmpty)
      assert(testCookie.get.name == "chousen")
      assert(testCookie.get.content == "test")


    }


    "Return the cookie using an extension method if one exists" in {

      implicit val extension: (Request) => chousenCookie.TokenSyntax = chousenCookie.TokenSyntax

      val testRequest = Request.apply(org.http4s.dsl.GET).addCookie("chousen", "test")

      val testCookie: Option[String] = testRequest.requestToken

      assert(testCookie.nonEmpty)
      assert(testCookie.get == "test")

    }

  }

}
