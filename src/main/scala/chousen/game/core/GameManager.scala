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
          else GameMessage(s"${p.name} encounters: ${es.toList.map(_.name).mkString(", ")}"))

        GameOps.updateUntilPlayerIsActive(p, es, m ++ msgs)
    }

    update(game)
  }

  override def takeCommand(command: CommandRequest, game: GameState): GameState = {

    command match {
      case AttackRequest(targetId) =>
        // This is promising, could be used generically?
        val executeAction = GameStateOps.targettedLens(targetId).modify {
          case (p, optE, msgs) =>

            optE match {
              case Some(e) =>
                // Not safe, could deal negative damage!
                val dmg = p.stats.strength + p.stats.dexterity - e.stats.vitality

                val targetMsg = GameMessage(s"${p.name} attacks ${e.name}!")
                val dmgMsg = GameMessage(s"${p.name} deals $dmg to ${e.name}!")

                // This should be replaced by a generic attack/damage function
                val newEnemy = EnemyOptics.charStats.composeLens(CharStatsOptics.hp)
                  .modify(hp => hp - dmg)(e)
                val gameMessages = msgs :+ targetMsg :+ dmgMsg

                (p.copy(position = p.position - 100), Option(newEnemy), gameMessages)
              case None => (p, optE, msgs)
            }
        }
        val completeTurn = encounterLens.modify(GameOps.updateUntilPlayerIsActive)
        val action = executeAction andThen completeTurn

        action(game)

      case SingleTargetActionRequest(_, _) => game
      case MultiTargetActionRequest(_, _) => game
    }
  }


}