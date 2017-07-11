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
    val dungeonSeed = new scala.util.Random().nextInt(6)
    val dungeonSeed2 = new scala.util.Random().nextInt(6)
    val dungeonSeed3 = new scala.util.Random().nextInt(6)

    val p = Player(name, "Rouge", CharStats(100, 100), Experience(), 0)

    val player = seed match {
      case 0 => SetPlayerStats.apply(2, 0, 0, 1).compose(PlayerClassLens.set("Warrior"))(p)
      case 1 => SetPlayerStats.apply(0, 3, 0, 0).compose(PlayerClassLens.set("Assassin"))(p)
      case 2 => SetPlayerStats.apply(-1, 0, 4, 0).compose(PlayerClassLens.set("Wizard"))(p)
      case 3 => SetPlayerStats.apply(0, 1, 1, 1).compose(PlayerClassLens.set("Jester"))(p)
      case 4 => SetPlayerStats.apply(1, 1, 0, 1).compose(PlayerClassLens.set("Barbarian"))(p)
      case _ => SetPlayerStats.apply(0, 0, 0, 2).compose(PlayerClassLens.set("Rogue"))(p)
    }

    val cards: Cards = seed match {
      case 0 => CardManager.startGame(CardCatalogue.strengthDeck)
      case 1 => CardManager.startGame(CardCatalogue.dexterityDeck)
      case 2 => CardManager.startGame(CardCatalogue.magicDeck)
      case 3 => CardManager.startGame(CardCatalogue.cheeseDeck)
      case 4 => CardManager.startGame(CardCatalogue.strongManDeck)
      case _ => CardManager.startGame(CardCatalogue.defaultDeck)
    }

    import chousen.Implicits._

    def campFire = Battle(Set(Enemy("Camp Fire", UUID.randomUUID(), CharStats(3, 3, strength = 6, speed = 0), 0)))


    // T1
    def mkEnemy(name: String, stats: CharStats) = Battle(Set(Enemy(name, UUID.randomUUID(), stats, 0)))
    def createSlime = mkEnemy("Slime", CharStats(13, 13, vitality = 6))
    def createSloth = mkEnemy("Sloth", CharStats(26, 26, strength = 12, vitality = 4, speed = 4))
    def createRat = mkEnemy("Rat", CharStats(7, 7, strength = 4, vitality = 4, speed = 12))

    // T2
    def gnoll = mkEnemy("Gnoll", CharStats(35, 35, strength = 8, dexterity = 8, vitality = 8))
    def giantWorm = mkEnemy("Giant Worm", CharStats(50, 50, strength = 15, vitality = 6, speed = 3))


    // T3
    def giantRat = mkEnemy("Giant Rat", CharStats(26, 26, dexterity = 9, vitality = 6, speed = 11))
    def oldOrc = mkEnemy("Old Orc", CharStats(70, 70, strength = 14, dexterity = 6, vitality = 10, speed = 4))
    def goblin = mkEnemy("Goblin", CharStats(50, 50, strength = 9, dexterity = 10, vitality = 9, speed = 9))
    def golem = mkEnemy("Golem", CharStats(100, 100, strength = 17, dexterity = 4, vitality = 15, speed = 2))


    // T4
    def warrior = mkEnemy("Warrior", CharStats(65, 65, strength = 17, dexterity = 10, vitality = 25))
    def orc = mkEnemy("Orc", CharStats(85, 85, strength = 22, dexterity = 7, vitality = 15, speed = 7))
    def troll = mkEnemy("Troll", CharStats(160, 160, strength = 42, intellect = 5, vitality = 14, speed = 2))

    // T5
    def orcPrince = mkEnemy("Orc Prince", CharStats(100, 100, strength = 25, vitality = 16, speed = 7))


    def orcKing1 = mkEnemy("Orc King", CharStats(130, 130, strength = 32, vitality = 20))
    def orcKing2 = mkEnemy("Orc King", CharStats(125, 125, strength = 28, vitality = 15, speed = 10))
    def orcKing3 = mkEnemy("Orc King", CharStats(135, 135, strength = 30, vitality = 30, speed = 7))


    val battle1 = createSloth

    val battle2 = dungeonSeed match {
      case (0 | 1 | 2) => createRat |+| createRat |+| createRat |+| createRat
      case (3 | 4) => createSlime |+| createSlime
      case _ => giantWorm
    }
    val battle3 = createSlime |+| campFire

    val battle4 = dungeonSeed2 match {
      case (0 | 1 | 2) => oldOrc |+| createRat |+| createSloth |+| createRat |+| createSlime
      case (3 | 4) => gnoll |+| gnoll
      case _ => oldOrc |+| createRat |+| createRat
    }

    val battle5 = dungeonSeed match {
      case (0 | 2 | 4) => golem |+| gnoll
      case (1 | 3) => giantWorm |+| giantWorm |+| createSloth
      case _ => warrior
    }

    val battle6 = dungeonSeed3 match {
      case (1 | 2 | 3) => orc |+| troll
      case (4 | 5) => golem |+| troll |+| golem
      case _ => warrior |+| troll
    }

    val battle8 = dungeonSeed2 match {
      case (5 | 4 | 3) => giantRat |+| goblin |+| giantRat
      case (2 | 1) => giantRat |+| giantRat |+| giantRat
      case _ => giantRat |+| golem |+| giantRat
    }

    val battle9 = dungeonSeed match {
      case (0 | 1 | 2) => warrior
      case (3 | 4) => orcPrince
      case _ => warrior |+| warrior
    }

    val battle10Left = dungeonSeed match {
      case (0 | 1 | 2) => troll
      case (3 | 4) => orc
      case _ => orcPrince
    }
    val battle10Right = dungeonSeed2 match {
      case (0 | 1 | 2) => troll
      case 3 => oldOrc |+| oldOrc
      case 4 => warrior |+| golem
      case _ => orcPrince
    }
    val boss = dungeonSeed3 match {
      case (0 | 1 ) => orcKing1
      case (2 | 3) => orcKing3
      case 4 => orcPrince |+| orcPrince
      case _ => orcKing2
    }

    val battle10 = battle10Left |+| boss |+| battle10Right


    val dungeon = Dungeon(battle1, LinearSeq(battle2, battle3, battle4, campFire,
      battle5, battle6, campFire,
      battle8, campFire,
      battle9, campFire,
      battle10))

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

    g.copy(cards = CardManager.drawCard(CardManager.drawCard(g.cards, limit = CardManager.PRE_DISCARD_MAX_HAND_SIZE)))
  }

  def startEncounterMessage(enemies: Set[Enemy], player: Player): GameMessage = {
    if (enemies.size == 1) GameMessage(s"${player.name} is attacked by ${enemies.head.name}!")
    else GameMessage(s"${player.name} is attacked by: ${enemies.toList.map(_.name).mkString(", ")}!")
  }

}
