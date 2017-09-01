package chousen

import org.http4s.client.blaze._
import org.scalatest.WordSpec

class Http4sServerSpec extends WordSpec {

  "Http4sServer" when {

    val serverBuilder = Http4sServer
    val httpClient = PooledHttp1Client()

    "When Assets are requested" should {
      lazy val result = httpClient.expect[String]("http://localhost:8080/assets/chousen.js").unsafeRun()

      "Returns the index page" in {
        val s = serverBuilder.buildServer.run
        assert(result.contains("function registerAttackButtton("))
        val _ = s.shutdownNow()
      }

    }
  }
}
