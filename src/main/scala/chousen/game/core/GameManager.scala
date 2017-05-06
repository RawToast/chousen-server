package chousen.game.core

import java.util.UUID

import chousen.api.data._
import chousen.game.actions.BasicAttack
import chousen.game.core.GameStateOptics.{EncounterLens, MessagesLens}

import scala.collection.LinearSeq

trait Command

trait ActionCalc

trait GameManager[A] {
  def create(name: String, uuid: UUID = UUID.randomUUID()): A

  def start(game: A): A

  def takeCommand(command: CommandRequest, game: A): A

  def transition(game: A): A
}

object GameStateManager extends GameManager[GameState] {

  import cats.syntax.all._

  override def create(name: String, uuid: UUID): GameState = {

    val player = Player(name, CharStats(100, 100), 0)
    val cards = Cards(List(Card("Fireball Card", "Casts a fireball, dealing damage to all enemies")))

    import chousen.api.types.Implicits._

    implicit def toBattle(e: Enemy): Battle = Battle(Set(e))

    def createSlime = Battle(Set(Enemy("Slime", UUID.randomUUID(), CharStats(10, 10), 0)))

    def createSloth = Battle(Set(Enemy("Sloth", UUID.randomUUID(), CharStats(17, 17, strength = 12, speed = 4), 0)))

    def createRat = Battle(Set(Enemy("Rat", UUID.randomUUID(), CharStats(7, 7, strength = 6, speed = 12), 0)))

    def orcus = battleMonoid.empty |+| Enemy("Orcus Malorcus", UUID.randomUUID(), CharStats(40, 40, strength = 11, speed = 5), 0)

    val battle1 = createRat |+| createRat
    val battle2 = createSlime |+| createSloth

    val dungeon = Dungeon(battle1, LinearSeq(battle2, orcus))
    val msgs = Seq.empty[GameMessage]

    GameState(uuid, player, cards, dungeon, msgs)
  }

  override def start(game: GameState): GameState = {
    val update = EncounterLens.modify {
      case (p: Player, es: Set[Enemy], m: Seq[GameMessage]) =>

        val msgs = Seq(GameMessage(s"${p.name} has entered the dungeon"),
          startEncounterMessage(es, p))

        GameOps.updateUntilPlayerIsActive(p, es, m ++ msgs)
    }

    update(game)
  }

  override def takeCommand(command: CommandRequest, game: GameState): GameState = {

    val newState = command match {
      case AttackRequest(targetId) => {
        val newState = GameTurnLoop.takeTurn(game, BasicAttack.attack(targetId))
        transition(newState)
      }
      case SingleTargetActionRequest(_, _) => game
      case MultiTargetActionRequest(_, _) => game
    }


    newState
  }

  override def transition(game: GameState): GameState = {

    val playerIsDead = game.player.stats.currentHp <= 0
    lazy val deathMessage = GameMessage(s"${game.player.name} dies.")
    lazy val winMessage = GameMessage(s"A winner is ${game.player.name}!")

    if (playerIsDead) MessagesLens.modify(msgs => msgs :+ deathMessage)(game)
    else {
      if (game.dungeon.currentEncounter.enemies.isEmpty && game.dungeon.remainingEncounters.isEmpty) MessagesLens.modify(msgs => msgs :+ winMessage)(game)
      else if (game.dungeon.currentEncounter.enemies.isEmpty && game.dungeon.remainingEncounters.nonEmpty) {
        GameStateOptics.DungeonTriLens.modify { (pdm: (Player, Dungeon, Seq[GameMessage])) =>
          val (p, d, msgs) = pdm
          val newDungeon = Dungeon(d.remainingEncounters.head, d.remainingEncounters.tail)
          val healAmount = 20

          val restMsg = GameMessage(s"${p.name} rests and recovers $healAmount hp.")
          val progressMsg = GameMessage(s"${p.name} recklessly wanders deeper into the dungeon.")
          val encounterMsg = startEncounterMessage(newDungeon.currentEncounter.enemies, p)

          val newPlayer = PlayerOptics.charStats.composeLens(CharStatsOptics.hp).modify(_ + healAmount)(p)

          (newPlayer, newDungeon, msgs :+ restMsg :+ progressMsg :+ encounterMsg)
        }(game)
      } else game
    }
  }

  def startEncounterMessage(enemies: Set[Enemy], player: Player): GameMessage = {
    if (enemies.size == 1) GameMessage(s"${player.name} is attacked by ${enemies.head.name}!")
    else GameMessage(s"${player.name} is attacked by: ${enemies.toList.map(_.name).mkString(", ")}!")
  }
}
