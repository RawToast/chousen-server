package chousen.http4s

import org.http4s
import org.http4s.headers.Cookie
import org.http4s.twirl.TwirlInstances

trait HtmlService extends TwirlInstances

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
