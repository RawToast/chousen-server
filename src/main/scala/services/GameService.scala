package services

import java.util.UUID

import controllers.CreateResponse

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

class DummyGameService extends GameService {
  lazy private val uuid = UUID.fromString("27a4ac0e-bae3-48fa-84ba-326ab52d071e")
  override def fetchGame(id: UUID): Future[CreateResponse] = if(id == uuid) Future(CreateResponse(uuid)) else Future.failed(new RuntimeException("Bad id"))

  override def initGame(name: String): Future[UUID] = Future(uuid)
}

trait GameService {

  def fetchGame(id: UUID): Future[CreateResponse]

  def initGame(name: String): Future[UUID]
}
