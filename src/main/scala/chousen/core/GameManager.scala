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

//override def create(name: String, uuid: UUID = UUID.randomUUID()): Game = {
//  val pc = PlayerCharacter.create(name)
//  val deck = DeckManager.startNewGameWithDefaultDeck
//  val dungeon = {
//  val firstEncounter = Encounter.create(EnemyCharacter.yellowSlime) + EnemyCharacter.slime
//  val secondEncounter = firstEncounter + EnemyCharacter.giantSlime
//  val thirdEncounter = Encounter.create(EnemyCharacter.scoundrel)
//  Dungeon(List(firstEncounter, secondEncounter, thirdEncounter))
//}
//
//  val msg = GameMessage(s"$name has entered the dungeon")
//
//  Game(uuid, pc, deck, dungeon, Seq(msg))
//}```````