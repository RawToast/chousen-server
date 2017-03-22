package chousen.core.old

import java.util.UUID

import api.data.{CharStats, GameMessage}
import chousen.{Cast, Peoples}
import chousen.cards.{Deck, DeckManager}
import chousen.character._
import chousen.core.GameManager
import chousen.engine.{ActionCalc, Engine, State}
import monocle.Lens
import chousen._

import scala.annotation.tailrec


object BasicGameManager extends GameManager[Game] {
  val actionCalc = Engine

  override def create(name: String, uuid: UUID = UUID.randomUUID()): Game = {
    val pc = PlayerCharacter.create(name)
    val deck = DeckManager.startNewGameWithDefaultDeck
    val dungeon = {
      val firstEncounter = Encounter.create(EnemyCharacter.yellowSlime) + EnemyCharacter.slime
      val secondEncounter = firstEncounter + EnemyCharacter.giantSlime
      val thirdEncounter = Encounter.create(EnemyCharacter.scoundrel)
      Dungeon(List(firstEncounter, secondEncounter, thirdEncounter))
    }

    val msg = GameMessage(s"$name has entered the dungeon")

    Game(uuid, pc, deck, dungeon, Seq(msg))
  }

  override def takeCommand(command: Command, game: Game): Game = {
      command.action match {
        case pa: PlayerAttack =>
          val allEnemies: Set[BaseCharacter] = game.quest.current.enemies
          val byStanders = Option(allEnemies -- command.target)
          val nc: Cast = pa.complete(game.player, command.target, byStanders)(actionCalc)

          val updatedQuest = Dungeon.current.set(Encounter(nc.enemies))(game.quest)

          update(Game(game.id, nc.player, game.deckManager, updatedQuest))
        case sp: CardAction => game
      }
    }

  private def update(game:Game): Game = {
    // reset player
    val positionUpdate = Game.player.composeLens(PlayerCharacter.posit).modify(i => i - 100)
    val updGame: Game = positionUpdate(game)

    // loop until active

    def loopDaLoop(peoples: Cast): Cast = peoples.active match {
      case _: PlayerCharacter => println("Player is active"); peoples
      case bc: BaseCharacter => {
        println("Enemy is active")

        val pl = peoples.player
        val enemies = peoples.enemies

        implicit val basicTargetting = (c:Cast) => Set(c.player.asInstanceOf[BaseCharacter]) -> Option(c.enemies - bc)

        def attackz(bc: BaseCharacter, p: Cast)(implicit ip: Cast => (Set[BaseCharacter], Option[Set[BaseCharacter]])): Cast = {
          val tar = ip(p)._1
          val bystanders = ip(p)._2
          bc.attack(tar, bystanders).changeTurn
        }

        type BasicUpdate = (PlayerCharacter, Set[BaseCharacter], Seq[GameMessage]) => (PlayerCharacter, Set[BaseCharacter], Seq[GameMessage])

        Game.ultLens.modify{case ((pc: PlayerCharacter, bcs:Set[BaseCharacter], msgs:Seq[GameMessage])) =>


          ???
        }




        val c: Cast = attackz(bc, peoples)
        loopDaLoop(c)
      }
    }
    val player: ((PlayerCharacter) => PlayerCharacter) => Game = Game.player.modify(_)(updGame)
    val enemies: ((Set[BaseCharacter]) => Set[BaseCharacter]) => Game = Game.currentEnemies.modify(_)(updGame)

    def mergeLens[S, A, B](lsa : Lens[S, A], lsb : Lens[S, B]) : Lens[S, (A, B)] =
      Lens.apply[S, (A, B)](s => (lsa.get(s), lsb.get(s)))(t => (lsa.set(t._1) andThen lsb.set(t._2)))

    val encounterLens: Lens[Game, (PlayerCharacter, Set[BaseCharacter])] = mergeLens(Game.player, Game.currentEnemies)

    val peeps: Cast = Peoples.init(updGame.player, updGame.quest.current.enemies)

    val nPeeps = loopDaLoop(peeps)

    Game.refreshFromCast(nPeeps)(updGame)
  }

  override def start(game: Game) = {
    val enc = game.quest.current

    val player: ((PlayerCharacter) => PlayerCharacter) => (Game) => Game = Game.player.modify
    val enemies: ((Set[BaseCharacter]) => Set[BaseCharacter]) => (Game) => Game = Game.currentEnemies.modify


    val peeps = Peoples.init(game.player, enc.enemies)

    Game.refreshFromCast(peeps)(game)
  }
}

case class Command(target: Set[BaseCharacter], action: Action)

trait PlayerAttack extends Action

object PlayerAttack extends PlayerAttack {
  override val name: String = "Attack"
  override val description: String = "Attacks all selected targets"

  override def complete(user: BaseCharacter, target: Set[BaseCharacter],
                        bystanders: Option[Set[BaseCharacter]])(calc: ActionCalc): Cast = {

    val t = target.map { e =>
      val damage = calc.calcDamage(user, e)
      // if (isPlayer) exclaim(s"$char deals $damage to $e")
      e.takeDamage(damage)
    }
    // Actors(char, t ++ bystanders.getOrElse(Set.empty))

    val all = t + user ++ bystanders.getOrElse(Set.empty)

    val player = all.find(p => p.isPlayer)

    Peoples(player.get.asInstanceOf[PlayerCharacter], all.filterNot(p => p.isPlayer))
  }
}



// OLD ENGINE
// For reference purposes
object GameObject {

  statement("Enter name: ")
  val name = requireCaseSensitivePlayerInput
  val initDeck = Deck.create

  val player = PlayerCharacter(name, UUID.randomUUID(), CharStats.DEFAULT)()

  val defaultDeck = DeckManager.startNewGameWithDefaultDeck

  val firstEncounter = Encounter.create(EnemyCharacter.yellowSlime) + EnemyCharacter.slime
  val secondEncounter = firstEncounter + EnemyCharacter.giantSlime
  val thirdEncounter = Encounter.create(EnemyCharacter.scoundrel)

  val dungeon = Dungeon(List(firstEncounter, secondEncounter, thirdEncounter))

  GameLoop(name).loop(player, defaultDeck, dungeon)
}




case class GameLoop(playerName: String) {
  story(s"$playerName has entered the dungeon")
  story(s"It was dark and smelly")
  statement(s"Eventually $playerName finds a room with a chest!")

  def loop(p: PlayerCharacter, deckManager: DeckManager, dungeon: Dungeon) = {

    break()
    @tailrec
    def innerLoop(actors: Cast, dm: DeckManager): State = {

      // Move
      val (newCast:Cast, nxtDm: DeckManager) = actors.takeTurn(dm)

      val state = newCast.postAttackState

      if (!state.playerAlive || !state.actors.hasEnemies) state
      else innerLoop(state.actors.changeTurn, nxtDm)
    }

    @tailrec
    def play(player: PlayerCharacter, d: Dungeon): Dungeon = {
      val encounterOption: Option[Encounter] = d.nextEncounter
      val encounter = encounterOption.get

      exclaim(encounter.toString)

      val cast = Peoples.init(player, encounter.enemies)

      // Fight
      val result: State = innerLoop(cast, deckManager)

      // Conclude
      val newDungeon = d.progress

      if (result.playerAlive && newDungeon.isNotComplete) play(result.actors.player, newDungeon)
      else d
    }

    if (play(p, dungeon).isNotComplete) {
      exclaim("Game over")
    } else {
      story(s"$playerName found a wooden chest")
      story(s"$playerName opens the wooden chest")
      break()
      suspense(s"Unfortunately")
      story(s" it was a trap and $playerName died")
    }
  }
}
