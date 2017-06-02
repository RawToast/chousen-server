package chousen.game.core

import java.util.UUID

import chousen.api.data.PlayerOptics.PlayerCharStatsLens
import chousen.api.data._
import chousen.game.actions.{BasicAttack, MultiTargetActionHandler, SelfActionHandler, SingleTargetActionHandler}
import chousen.game.cards.{CardCatalogue, CardManager}
import chousen.game.core.GameStateManager.startEncounterMessage
import chousen.game.core.GameStateOptics.{EncounterLens, MessagesLens}

import scala.annotation.tailrec
import scala.collection.LinearSeq

trait GameManager[A] {
  def create(name: String, uuid: UUID = UUID.randomUUID()): A

  def start(game: A): A

  def takeCommand(command: CommandRequest, game: A): A

  def transition(game: A): A

  def useCard(card: Card, commandRequest: CommandRequest, game: A): A
}

trait GameStateCreation {
  def createAndStart(name: String, uuid: UUID = UUID.randomUUID()): GameState = {
    start(create(name, uuid))
  }

  def create(name: String, uuid: UUID = UUID.randomUUID()): GameState = {
    import cats.syntax.all._
    val seed = new scala.util.Random().nextInt(2)
    val p = Player(name, CharStats(100, 100), 0)

    val player = seed match {
      case 0 => PlayerOptics.PlayerStrengthLens.modify(_+2)(p)
      case 1 => PlayerOptics.PlayerDexterityLens.modify(_+2)(p)
      case 2 => PlayerOptics.PlayerIntellectLens.modify(_+2)(p)
      case _ => PlayerOptics.PlayerVitalityLens.modify(_+2)(p)
    }

    val cards = seed match {
      case 0 => CardManager.startGame(CardCatalogue.strengthDeck)
      case 1 => CardManager.startGame(CardCatalogue.dexterityDeck)
      case 2 => CardManager.startGame(CardCatalogue.magicDeck)
      case _ => CardManager.startGame(CardCatalogue.defaultDeck)
    }
    //val cards = CardManager.startGame(CardCatalogue.defaultDeck)

    import chousen.api.types.Implicits._

    implicit def toBattle(e: Enemy): Battle = Battle(Set(e))

    def campFire = Battle(Set(Enemy("Camp Fire", UUID.randomUUID(), CharStats(3, 3, strength = 6, speed = 0), 0)))

    def createSlime = Battle(Set(Enemy("Slime", UUID.randomUUID(), CharStats(12, 12), 0)))

    def createSloth = Battle(Set(Enemy("Sloth", UUID.randomUUID(), CharStats(23, 23, strength = 12, speed = 4), 0)))

    def createRat = Battle(Set(Enemy("Rat", UUID.randomUUID(), CharStats(7, 7, strength = 4, speed = 12), 0)))

    def orc = battleMonoid.empty |+| Enemy("Orc", UUID.randomUUID(), CharStats(80, 80, strength = 12, vitality = 11, speed = 7), 0)

    def giantOrc = battleMonoid.empty |+| Enemy("Giant Orc", UUID.randomUUID(), CharStats(130, 130, strength = 40, vitality = 14, speed = 2), -10)

    val battle1 = createSloth
    val battle2 = createRat |+| createRat |+| createRat |+| createRat
    val battle3 = createSlime |+| campFire

    val battle4 = createSlime |+| createSloth |+| createSlime
    val battle5 = orc |+| createRat |+| createSloth |+| createRat |+| createSlime
    val battle6 = campFire

    val battle7 = orc |+| giantOrc


    val dungeon = Dungeon(battle1, LinearSeq(battle2, battle3, battle4, battle5, battle6, battle7))
    val msgs = Seq.empty[GameMessage]

    GameState(uuid, player, cards, dungeon, msgs)
  }

  def start(game: GameState): GameState = {
    val update = EncounterLens.modify {
      case (p: Player, es: Set[Enemy], m: Seq[GameMessage]) =>

        val msgs = Seq(GameMessage(s"${p.name} has entered the dungeon"),
          startEncounterMessage(es, p))

        GameOps.updateUntilPlayerIsActive(p, es, m ++ msgs)
    }

    update(game)
  }
}

object GameStateManager extends GameManager[GameState] with GameStateCreation {

  override def useCard(card: Card, commandRequest: CommandRequest, game: GameState): GameState = {

    CardManager.playCard(card) { (c: Card) =>
      if (commandRequest match {
        case AttackRequest(_) => false
        case SelfInflictingActionRequest(action) => c.action == action
        case SingleTargetActionRequest(_, action) => c.action == action
        case MultiTargetActionRequest(_, action) => c.action == action
      }) takeCommand(commandRequest, game)
      else game
    }.apply(game)
  }

  override def takeCommand(command: CommandRequest, game: GameState): GameState = {
    val newState = command match {
      case SelfInflictingActionRequest(a) =>
        val ns = GameTurnLoop.takeTurn(game,
          SelfActionHandler.handle(a).apply)
          transition(ns)
      case AttackRequest(targetId) =>
        val newState = GameTurnLoop.takeTurn(game, BasicAttack.attack(targetId))
        transition(newState)
      case SingleTargetActionRequest(targetId, action) =>
        val ns= GameTurnLoop.takeTurn(game,
          SingleTargetActionHandler.handle(targetId, action))
        transition(ns)
      case MultiTargetActionRequest(targets, action) =>
        val ns = GameTurnLoop.takeTurn(game,
          MultiTargetActionHandler.handle(targets, action))
        transition(ns)
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
        val g = GameStateOptics.DungeonTriLens.modify { (pdm: (Player, Dungeon, Seq[GameMessage])) =>
          val (p, d, msgs) = pdm
          val newDungeon = Dungeon(d.remainingEncounters.head, d.remainingEncounters.tail)
          val healAmount = Math.min(Math.max(0, p.stats.maxHp - p.stats.currentHp), 20)

          val restMsg = GameMessage(s"${p.name} rests and recovers $healAmount hp.")
          val strongerMsg = GameMessage(s"${p.name} feels stronger after resting.")
          val progressMsg = GameMessage(s"${p.name} recklessly wanders deeper into the dungeon.")
          val encounterMsg = startEncounterMessage(newDungeon.currentEncounter.enemies, p)

          val newPlayer = PlayerCharStatsLens.composeLens(CharStatsOptics.hp).modify(_ + healAmount).compose(
          PlayerCharStatsLens.modify(cs =>
            cs.copy(strength = cs.strength + 1, dexterity = cs.dexterity + 1, vitality = cs.vitality + 1))).apply(p)
          (newPlayer, newDungeon, msgs :+ restMsg :+ strongerMsg :+ progressMsg :+ encounterMsg)
        }.andThen(GameStateOptics.EncounterLens.modify(GameOps.updateUntilPlayerIsActive)).apply(game)

        @tailrec
        def refillHand(cards: Cards): Cards = {
          if (cards.hand.size >= 7) cards
          else refillHand(CardManager.drawCard(cards))
        }
        val cards = refillHand(g.cards)
        g.copy(cards = cards)
      } else game
    }
  }

  def startEncounterMessage(enemies: Set[Enemy], player: Player): GameMessage = {
    if (enemies.size == 1) GameMessage(s"${player.name} is attacked by ${enemies.head.name}!")
    else GameMessage(s"${player.name} is attacked by: ${enemies.toList.map(_.name).mkString(", ")}!")
  }

}
