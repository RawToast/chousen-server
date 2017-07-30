package chousen

import org.http4s.client.blaze._
import org.scalatest.WordSpec

class Http4sServerSpec extends WordSpec {

  "Http4sServer" when {

    val serverBuilder = Http4sServer
    val httpClient = PooledHttp1Client()

    "When root is requested" should {
      lazy val result = httpClient.expect[String]("http://localhost:8080/").unsafeRun()

      "Return the index page" in {
        val s = serverBuilder.buildServer.run
        assert(result.contains("<html"))
        assert(result.contains("<body>"))
        assert(result.contains("Chousen Dev Client"))
        val _ = s.shutdownNow()
      }

    }

  }

}
