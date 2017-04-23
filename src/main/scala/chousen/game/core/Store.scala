package chousen.game.core

import java.util.UUID

import chousen.api.data.GameState


trait Store[A, B] {

  val key: B => A

  def store(b: B): Map[A, B]

  def load(a: A): Option[B]
}

// Fake DB, awful mutation!
object GameStore extends Store[UUID, GameState] {

  private var db = Map.empty[UUID, GameState]

  override val key: (GameState) => UUID = _.id

  override def store(b: GameState): Map[UUID, GameState] = {
    db = db + (key(b) -> b)
    db
  }

  override def load(a: UUID): Option[GameState] = {
    db.get(a)
  }
}


