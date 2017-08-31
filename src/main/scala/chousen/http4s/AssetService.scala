package chousen.http4s

import fs2.Task
import org.http4s.{Request, HttpService, StaticFile}
import org.http4s.dsl.{->, /, GET, NotFound, NotFoundSyntax, Root}


class AssetService {
  private def static(file: String, request: Request) =
    StaticFile.fromResource("/" + file, Some(request)).map(Task.now).getOrElse(NotFound())

  val routes: HttpService = HttpService {
    case request @ GET -> Root / "assets" / path if List(".js", ".css").exists(path.endsWith) =>
      static(path, request)
  }
}
