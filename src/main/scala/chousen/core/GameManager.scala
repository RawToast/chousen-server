package chousen.core

import chousen.{core, _}
import chousen.cards.{Deck, DeckManager}
import chousen.character.{Action, BaseCharacter, CardAction, EnemyCharacter, PlayerCharacter}
import chousen.data.{CharStats, GameMessage}
import chousen.engine.{ActionCalc, Engine, State}

import scala.annotation.tailrec


object BasicGameManager extends GameManager {
  val actionCalc = Engine

  override def create(name: String): Game = {
    val pc = PlayerCharacter.create(name)
    val deck = DeckManager.startNewGameWithDefaultDeck
    val dungeon = {
      val firstEncounter = Encounter.create(EnemyCharacter.yellowSlime) + EnemyCharacter.slime
      val secondEncounter = firstEncounter + EnemyCharacter.giantSlime
      val thirdEncounter = Encounter.create(EnemyCharacter.scoundrel)
      core.Dungeon(List(firstEncounter, secondEncounter, thirdEncounter))
    }

    val msg = GameMessage(s"$name has entered the dungeon")

    Game(pc, deck, dungeon, Seq(msg))
  }

  override val takeCommand: (Command, Game) => Game =
    (com, gam) => {
      com.action match {
        case pa: PlayerAttack => {
          val allEnemies: Set[BaseCharacter] = gam.quest.current.enemies
          val byStanders = Option(allEnemies -- com.target)
          val nc: Cast = pa.complete(gam.playerCharacter, com.target, byStanders)(actionCalc)

          val updatedQuest = Dungeon.update.set(Encounter(nc.enemies))(gam.quest)

          Game(nc.player, gam.deckManager, updatedQuest)
        }
        case sp: CardAction => gam
      }
    }
}

trait GameManager {
  def create(name:String): Game

  val takeCommand: (Command, Game) => Game

  val actionCalc: ActionCalc
}

case class Game(playerCharacter: PlayerCharacter, deckManager: DeckManager,
                quest: Dungeon, messages: Seq[GameMessage] = Seq.empty)

case class Command(target: Set[BaseCharacter], action: Action)


class PlayerAttack extends Action {
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

  val player = PlayerCharacter(name, CharStats.DEFAULT)()

  val defaultDeck = DeckManager.startNewGameWithDefaultDeck

  val firstEncounter = Encounter.create(EnemyCharacter.yellowSlime) + EnemyCharacter.slime
  val secondEncounter = firstEncounter + EnemyCharacter.giantSlime
  val thirdEncounter = Encounter.create(EnemyCharacter.scoundrel)

  val dungeon = core.Dungeon(List(firstEncounter, secondEncounter, thirdEncounter))

  core.GameLoop(name).loop(player, defaultDeck, dungeon)
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
