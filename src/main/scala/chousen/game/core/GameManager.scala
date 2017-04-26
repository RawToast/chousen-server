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

  def takeCommand(command: CommandRequest, game: A): A
}


object GameStateManager extends GameManager[GameState] {

  import cats.syntax.all._
  import chousen.api.types.Implicits._

  private val encounterLens: Lens[GameState, (Player, Set[Enemy], Seq[GameMessage])] =
    LensUtil.triLens(GenLens[GameState](_.player),
      GenLens[GameState](_.dungeon.currentEncounter.enemies),
      GenLens[GameState](_.messages))

  override def create(name: String, uuid: UUID): GameState = {

    val player = Player(name, CharStats(100, 100), 0)
    val cards = Cards(List(Card("Fireball Card", "Casts a fireball, dealing damage to all enemies")))

    def createSlime = Battle(Set(Enemy("Slime", UUID.randomUUID(), CharStats(10, 10), 0)))

    def createBattle = createSlime |+| createSlime

    val dungeon = Dungeon(createBattle, LinearSeq(createBattle, createBattle |+| createBattle))
    val msgs = Seq.empty[GameMessage]

    GameState(uuid, player, cards, dungeon, msgs)
  }

  override def start(game: GameState): GameState = {
    val update = encounterLens.modify {
      case (p: Player, es: Set[Enemy], m: Seq[GameMessage]) =>

        val msgs = Seq(GameMessage(s"${p.name} has entered the dungeon"),
          if (es.size == 1) GameMessage(s"${p.name} encounters a ${es.head.name}!")
          else GameMessage(s"${p.name} encounters: ${es.map(_.name).mkString(", ")}"))

        GameOps.update(p, es, m ++ msgs)
    }

    update(game)
  }

  override def takeCommand(command: CommandRequest, game: GameState): GameState = {

    command match {
      case AttackRequest(targetId) =>
        game.dungeon.currentEncounter.enemies.find(e=> e.id == targetId) match {
          case Some(_) => {
            game
          }
          case None => game
        }
      case SingleTargetActionRequest(_, _) => game
      case MultiTargetActionRequest(_, _) => game
    }
  }


}