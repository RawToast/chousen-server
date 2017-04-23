package chousen.game.core

import java.util.UUID

import chousen.api.data._
import chousen.util.LensUtil
import monocle.Lens
import monocle.macros.GenLens

import scala.collection.LinearSeq

trait Command
trait ActionCalc

trait GameManager[A] {
  def create(name: String, uuid: UUID = UUID.randomUUID()): A

  def start(game: A): A

  def takeCommand(command: Command, game: A): A
}


object GameStateManager extends GameManager[GameState] {
  import chousen.api.types.Implicits._
  import cats.syntax.all._

  override def create(name: String, uuid: UUID): GameState = {

    val player = Player(name, CharStats(100, 100), 0)
    val cards = Cards(List(Card("Fireball Card", "Casts a fireball, dealing damage to all enemies")))
    def createSlime = Battle(Seq(Enemy("Slime", UUID.randomUUID(), CharStats(10, 10), 0)))
    def createBattle = createSlime |+| createSlime

    val dungeon = Dungeon(createBattle, LinearSeq(createBattle, createBattle |+| createBattle))
    val msgs = Seq.empty[GameMessage]

    GameState(uuid, player, cards, dungeon, msgs)
  }

  override def start(game: GameState): GameState = {
    val update = encounterLens.modify {
      case (p: Player, es: Seq[Enemy], m: Seq[GameMessage]) => GameOps.update(p, es, m)
    }

    update(game)
  }

  override def takeCommand(command: Command, game: GameState): GameState = game //FIXME: Implement

  private val encounterLens: Lens[GameState, (Player, Seq[Enemy], Seq[GameMessage])] =
    LensUtil.triLens(GenLens[GameState](_.player),
      GenLens[GameState](_.dungeon.currentEncounter.enemies),
      GenLens[GameState](_.messages))
}