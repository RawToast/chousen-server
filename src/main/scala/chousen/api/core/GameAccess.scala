package chousen.api.core

import java.util.UUID

import chousen.api.data._
import com.mongodb.async.client.{FindIterable, MongoIterable}
import fs2.Task
import org.lyranthe.fs2_mongodb.Mongo
import org.mongodb.scala.MongoClient
//import io.finch.Output
import org.http4s.Response

trait GameAccess[T] {
  def withGame(id: UUID)(f: GameState => T): T
}

//trait MappedGameAccess extends GameAccess[Output[GameState]] {
//  var store = Map.empty[UUID, GameState]
//
//  def withGame(id: UUID)(f: GameState => Output[GameState]): Output[GameState] = {
//    store.get(id) match {
//      case Some(game) => f(game)
//      case None => io.finch.NotFound(
//        new java.util.NoSuchElementException(s"Game with ID=$id does not exist"))
//    }
//  }
//}

trait Http4sMappedGameAccess extends GameAccess[Task[Response]] {
  import io.circe.generic.auto._
  import io.circe.syntax._
  import org.http4s.circe._
  import org.http4s.dsl._
  var store = Map.empty[UUID, GameState]

  store = store + (GameStateGenerator.uuid -> GameStateGenerator.staticGameState)

  def withGame(id: UUID)(f: GameState => Task[Response]): Task[Response] = {

    case class Error(msg: String)

    store.get(id) match {
      case Some(game) => f(game)
      case None => NotFound(Error(s"Game with ID=$id does not exist").asJson)
    }
  }
}


trait MongoGameAccess extends GameAccess[Task[Response]] {
  import fs2._
  import org.bson.Document
  import org.bson.codecs.configuration.CodecRegistries.{fromProviders, fromRegistries}
  import org.bson.codecs.configuration.CodecRegistry
  import org.lyranthe.fs2_mongodb.ScalaMongo
  import org.mongodb.scala.MongoDatabase
  import org.mongodb.scala.{FindObservable, MongoCollection}
  import org.mongodb.scala.bson.codecs.DEFAULT_CODEC_REGISTRY
  import org.mongodb.scala.bson.codecs.Macros._
  import org.mongodb.scala.model.Filters._
  import org.lyranthe.fs2_mongodb.implicits._


  lazy val codecRegistry: CodecRegistry = fromRegistries(fromProviders(classOf[GameState]), DEFAULT_CODEC_REGISTRY)

  implicit val strategy =
    Strategy.fromExecutionContext(scala.concurrent.ExecutionContext.global)

//  lazy val db: Stream[GameState, MongoDatabase] =
//    Mongo.client("mongodb://ds123080.mlab.com:23080")
//      .map(_.getDatabase("heroku_rm14s281")
//        .withCodecRegistry(codecRegistry))

  lazy val collection: Stream[GameState, MongoDatabase] = {
    val id = 1
    val db = ScalaMongo.sclient("mongodb://ds123080.mlab.com:23080")
      .map(_.getDatabase("heroku_rm14s281")
        .withCodecRegistry(codecRegistry))

    val collection: Stream[Nothing, MongoCollection[GameState]] = db.map(md => md.getCollection("chousen"))

    val allDocuments: Stream[Task, Document] =
      Mongo.client("mongodb://localhost")
        .flatMap(_.getDatabase("dbname").getCollection("collname").find().stream[Task])

//    val rr: Stream[Nothing, FindObservable[GameState]] = collection.map{ col =>
//      col.find(equal("id", id))}.stream[Task]


    def getGS: Stream[Nothing, MongoCollection[GameState]] = {
      ScalaMongo.sclient("mongodb://ds123080.mlab.com:23080")
        .map(_.getDatabase("heroku_rm14s281")
          .withCodecRegistry(codecRegistry)
          .getCollection("chousen"))
    }


  def fetchPreferences(srn: String) = {
    import org.lyranthe.fs2_mongodb.implicits._
    lazy val mongoClient: MongoClient = MongoClient("mongodb://ds123080.mlab.com:23080")

    val collection: FindObservable[GameState] = mongoClient.getDatabase("heroku_rm14s281")
      .withCodecRegistry(codecRegistry)
      .getCollection("chousen")
      .find(equal("id", srn))

    val iterable: FindIterable[GameState] = collection.getClass.getDeclaredField("wrapped")
      .get(collection).asInstanceOf[FindIterable[GameState]]gi

    iterable.stream[Task]
  }



  val allDocuments: Stream[Task, Document] =
    Mongo.client("mongodb://ds123080.mlab.com:23080")
      .flatMap(_.getDatabase("heroku_rm14s281")
        .withCodecRegistry(codecRegistry)
        .getCollection("chousen").find().stream[Task])

  def withGame(id: UUID)(f: GameState => Task[Response]): Task[Response] = {



  }


}