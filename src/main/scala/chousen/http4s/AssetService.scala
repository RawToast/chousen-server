package chousen.http4s

import fs2.Task
import fs2.interop.cats._
import org.http4s.dsl.{->, /, GET, NotFound, NotFoundSyntax, Root}
import org.http4s.{HttpService, Request, Response, StaticFile}

class AssetService {
  private def static(file: String, request: Request): Task[Response] =
    StaticFile.fromResource("/" + file, Some(request)).getOrElseF(NotFound())

  val routes: HttpService = HttpService {
    case request @ GET -> Root / "assets" / path if List(".js", ".css").exists(path.endsWith) =>
      static(path, request)
  }
}
