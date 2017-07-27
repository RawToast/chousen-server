package chousen.api.core

import java.util.UUID

import chousen.api.data.GameState
import fs2.{Strategy, Task}
import io.circe.Json
import io.circe.generic.auto._
import io.circe.parser._
import io.circe.syntax._
import org.bson.Document
import org.mongodb.scala.model.Filters._
import org.mongodb.scala.{MongoClient, MongoCollection, Observable, model}

import scala.concurrent.ExecutionContext

class MongoDatastore(connectionString: String, databaseName: String="heroku_rm14s281", collectionName: String="chousen") {

  implicit val executionContext: ExecutionContext = scala.concurrent.ExecutionContext.Implicits.global

  implicit val strategy: Strategy = Strategy.fromExecutionContext(executionContext)

  lazy val collection: MongoCollection[Document] =
    MongoClient(connectionString)
      .getDatabase(databaseName)
      .getCollection(collectionName)

  def get(id :UUID): Task[GameState] = for {
    document <- getDocument(id)
    hs <- toGameState(document)
  } yield hs

  def put(g: GameState): Task[GameState] =
    collection.replaceOne(equal("uuid", g.uuid.toString), Document.parse(g.asJson.spaces2),
      model.UpdateOptions().upsert(true)).asTask
      .map(_ => g)


  private def getDocument(id: UUID): Task[Document] = collection.find(equal("uuid", id.toString)).asTask

  private def toGameState(task: Document): Task[GameState] = {
    for {
      json <- parseJson(task.toJson)
      gameState <- parseGameState(json)
    } yield gameState
  }

  private def parseJson(s: String): Task[Json] = parse(s) match {
    case Left(err) => Task.fail(err.underlying)
    case Right(json) => Task.now(json)
  }

  private def parseGameState(j : Json): Task[GameState] = j.as[GameState] match {
    case Left(err) => Task.fail(err.getCause)
    case Right(json) => Task.now(json)
  }

  private implicit class TaskSyntax[T](obs: Observable[T]) {
    def asTask: Task[T] = Task.fromFuture(obs.head())
  }
}