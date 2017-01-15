package chousen.core

import java.util.UUID


trait Store[A, B] {

  val key: B => A

  def store(b: B): Map[A, B]

  def load(a: A): Option[B]
}

// Fake DB, awful mutation!
object GameStore extends Store[UUID, Game] {

  private var db = Map.empty[UUID, Game]

  override val key: (Game) => UUID = _.id

  override def store(b: Game): Map[UUID, Game] = {
    db = db + (key(b) -> b)
    db
  }

  override def load(a: UUID): Option[Game] = {
    db.get(a)
  }
}


