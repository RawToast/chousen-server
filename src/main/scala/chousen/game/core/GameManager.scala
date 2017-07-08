package chousen.game.core

import java.util.UUID

import chousen.api.data.PlayerOptics._
import chousen.api.data._
import chousen.game.actions.{BasicAttack, CardActionHandler, MultiTargetActionHandler, SelfActionHandler, SingleTargetActionHandler}
import chousen.game.cards.{CardCatalogue, CardManager}
import chousen.game.core.GameStateManager.startEncounterMessage
import chousen.game.core.GameStateOptics.{EncounterLens, MessagesLens}

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
    val seed = new scala.util.Random().nextInt(6)
    val p = Player(name, CharStats(100, 100), 0)

    val player = seed match {
      case 0 => SetPlayerStats.apply(2, 0, 0, 1)(p)
      case 1 => SetPlayerStats.apply(0, 3, 0, 0)(p)
      case 2 => SetPlayerStats.apply(-1, 0, 4, 0)(p)
      case 3 => SetPlayerStats.apply(0, 1, 1, 1)(p)
      case 4 => SetPlayerStats.apply(1, 1, 0, 1)(p)
      case _ => SetPlayerStats.apply(0, 0, 0, 2)(p)
    }

    val cards: Cards = seed match {
      case 0 => CardManager.startGame(CardCatalogue.strengthDeck)
      case 1 => CardManager.startGame(CardCatalogue.dexterityDeck)
      case 2 => CardManager.startGame(CardCatalogue.magicDeck)
      case 3 => CardManager.startGame(CardCatalogue.cheeseDeck)
      case 4 => CardManager.startGame(CardCatalogue.strongManDeck)
      case _ => CardManager.startGame(CardCatalogue.defaultDeck)
    }

    import chousen.api.types.Implicits._

    def toBattle(e: Enemy): Battle = Battle(Set(e))

    def campFire = Battle(Set(Enemy("Camp Fire", UUID.randomUUID(), CharStats(3, 3, strength = 6, speed = 0), 0)))

    def createSlime = Battle(Set(Enemy("Slime", UUID.randomUUID(), CharStats(13, 13, vitality = 6), 0)))

    def createSloth = Battle(Set(Enemy("Sloth", UUID.randomUUID(), CharStats(26, 26, strength = 12, vitality = 4, speed = 4), 0)))

    def createRat = Battle(Set(Enemy("Rat", UUID.randomUUID(), CharStats(7, 7, strength = 4, vitality = 4, speed = 12), 0)))
    def giantRat = Battle(Set(Enemy("Giant Rat", UUID.randomUUID(), CharStats(26, 26, dexterity = 9, vitality = 6, speed = 11), 0)))

    def oldOrc = toBattle(Enemy("Old Orc", UUID.randomUUID(), CharStats(70, 70, strength = 14, dexterity = 6, vitality = 10, speed = 4), 0))

    def orc = toBattle(Enemy("Orc", UUID.randomUUID(), CharStats(85, 85, strength = 20, dexterity = 7, vitality = 13, speed = 7), 0))

    def goblin = toBattle(Enemy("Goblin", UUID.randomUUID(), CharStats(50, 50, strength = 9, dexterity = 10, vitality = 9, speed = 9), 0))

    def oldWarrior = toBattle(Enemy("Old Warrior", UUID.randomUUID(), CharStats(60, 60, strength = 15, dexterity = 10, vitality = 22), 0))

    def troll = toBattle(Enemy("Troll", UUID.randomUUID(), CharStats(160, 160, strength = 40, intellect = 5, vitality = 14, speed = 2), 0))

    def orcKing = toBattle(Enemy("Orc King", UUID.randomUUID(), CharStats(130, 130, strength = 28, vitality = 17), -25))

    val battle1 = createSloth
    val battle2 = createRat |+| createRat |+| createRat |+| createRat
    val battle3 = createSlime |+| campFire

    val battle4 = oldOrc |+| createRat |+| createSloth |+| createRat |+| createSlime
    val battle5 = campFire

    val battle6 = orc |+| troll
    val battle7 = oldOrc |+| campFire

    val battle8 = giantRat |+| goblin |+| giantRat |+| campFire

    val battle9 = oldWarrior
    val battle10 = troll |+| orcKing |+| troll


    val dungeon = Dungeon(battle1, LinearSeq(battle2, battle3, battle4, battle5, battle6, battle7, battle8, battle9, battle10))
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
        case CardActionRequest(action) => c.action == action
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
      case CardActionRequest(action) =>
        val ns = GameTurnLoop.takeTurn(game, CardActionHandler.handle(action))
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

        postTurn(game)

      } else game
    }
  }

  private def postTurn(gs:GameState): GameState = {

    val g = GameStateOptics.DungeonTriLens.modify { (pdm: (Player, Dungeon, Seq[GameMessage])) =>
      val (p, d, msgs) = pdm
      val newDungeon = Dungeon(d.remainingEncounters.head, d.remainingEncounters.tail)
      val healAmount = Math.min(Math.max(0, p.stats.maxHp - p.stats.currentHp), 20)

      val restMsg = GameMessage(s"${p.name} rests and recovers $healAmount hp.")
      val strongerMsg = GameMessage(s"${p.name} feels stronger after resting.")
      val progressMsg = GameMessage(s"${p.name} recklessly wanders deeper into the dungeon.")
      val encounterMsg = startEncounterMessage(newDungeon.currentEncounter.enemies, p)

      val newPlayer = PlayerCharStatsLens.composeLens(CharStatsOptics.HpLens).modify(_ + healAmount).apply(p)
      (newPlayer, newDungeon, msgs :+ restMsg :+ strongerMsg :+ progressMsg :+ encounterMsg)
    }.andThen(GameStateOptics.EncounterLens.modify(GameOps.updateUntilPlayerIsActive)).apply(gs)

    g.copy(cards = CardManager.drawCard(CardManager.drawCard(g.cards)))
  }

  def startEncounterMessage(enemies: Set[Enemy], player: Player): GameMessage = {
    if (enemies.size == 1) GameMessage(s"${player.name} is attacked by ${enemies.head.name}!")
    else GameMessage(s"${player.name} is attacked by: ${enemies.toList.map(_.name).mkString(", ")}!")
  }

}
