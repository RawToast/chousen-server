package chousen.http4s

import fs2.Task
import org.http4s._
import org.http4s.dsl._

class AssetService {
  private def static(file: String, request: Request) =
    StaticFile.fromResource("/" + file, Some(request)).map(Task.now).getOrElse(NotFound())
  // static: (file: String, request: org.http4s.Request)scalaz.concurrent.Task[org.http4s.Response]

  val routes = HttpService {
    case request @ GET -> Root / "assets" / path if List(".js", ".css").exists(path.endsWith) =>
      static(path, request)
  }
}
