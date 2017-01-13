import com.twitter.util.Await
import io.finch._
import com.twitter.finagle.Http

/**
  * A tiny Finch application that serves a single endpoint `GET /hello` that returns
  * HTTP 200 response saying "Hello, World!"
  *
  * Use the following sbt command to run the application.
  *
  * {{{
  *   $ sbt run Main
  * }}}
  *
  * Use the following HTTP commands to test endpoints.
  *
  * {{{
  *   $ http GET :8080/crit
  * }}}
  */
object Main extends App {

  val api: Endpoint[String] = get("crit" / string) { name: String =>
    Ok(s"$name got crit!")
  }

  Await.ready(Http.serve(":8080", api.toServiceAs))
}
