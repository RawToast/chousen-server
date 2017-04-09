package chousen.core

import java.util.UUID

import api.data._
import chousen.core.old.Command
import chousen.engine.{ActionCalc, Engine}

import scala.collection.LinearSeq

trait GameManager[A] {
  def create(name: String, uuid: UUID = UUID.randomUUID()): A

  def start(game: A): A

  def takeCommand(command: Command, game: A): A

  val actionCalc: ActionCalc
}


object GameStateManager extends GameManager[GameState] {
  import api.types.Implicits._
  import cats.syntax.all._

  override def create(name: String, uuid: UUID): GameState = {

    val player = Player(name, CharStats(100, 100), 0)
    val cards = Cards(List(Card("Fireball Card", "Casts a fireball, dealing damage to all enemies")))
    def createSlime = Battle(Set(Enemy("Slime", UUID.randomUUID(), CharStats(10, 10), 0)))
    def createBattle = createSlime |+| createSlime

    val dungeon = Dungeon(createBattle, LinearSeq(createBattle, createBattle |+| createBattle))
    val msgs = Seq.empty[GameMessage]

    GameState(uuid, player, cards, dungeon, msgs)
  }

  override def start(game: GameState): GameState = game //FIXME: Implement

  override def takeCommand(command: Command, game: GameState): GameState = game //FIXME: Implement

  override val actionCalc: ActionCalc = Engine //FIXME: Implement
}