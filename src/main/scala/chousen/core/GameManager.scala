package chousen.core

import java.util.UUID

import api.data._
import chousen.core.old.Command
import chousen.engine.ActionCalc

trait GameManager[A] {
  def create(name: String, uuid: UUID = UUID.randomUUID()): A

  def start(game: A): A

  def takeCommand(command: Command, game: A): A

  val actionCalc: ActionCalc
}


trait GameStateManager extends GameManager[GameState] {
  override def create(name: String, uuid: UUID): GameState = {

    val player = Player(name, CharStats(100, 100), 0)
    val cards = Cards(List(Card("Fireball Card", "Casts a fireball, dealing damage to all enemies")))
    val dungeon = ???
    val msgs = ???

    GameState(uuid, player, cards, dungeon, msgs)
  }

  override def start(game: GameState): GameState = ???

  override def takeCommand(command: Command, game: GameState): GameState = ???

  override val actionCalc: ActionCalc = ???
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