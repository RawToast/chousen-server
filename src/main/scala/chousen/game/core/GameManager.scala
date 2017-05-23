package chousen.game.core

import java.util.UUID

import chousen.api.data.PlayerOptics.PlayerCharStatsLens
import chousen.api.data._
import chousen.game.actions.{BasicAttack, MultiTargetActionHandler, SelfActionHandler, SingleTargetActionHandler}
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

    val player = Player(name, CharStats(100, 100), 0)
    val cards = Cards(List(Card(UUID.randomUUID(), "Crushing Blow", "Deals heavy damage, but has an increased movement penalty ", CrushingBlow),
      Card(UUID.randomUUID(), "Crushing Blow", "Deals heavy damage, but has an increased movement penalty", CrushingBlow),
      Card(UUID.randomUUID(), "Quick Attack", "Attack with reduced movement penalty", QuickAttack),
      Card(UUID.randomUUID(), "Quick Attack", "Attack with reduced movement penalty", QuickAttack),
      Card(UUID.randomUUID(), "Heal Wounds", "Heals 30HP", HealWounds),
      Card(UUID.randomUUID(), "Fireball", "Deals fire damage to all enemies", Fireball),
      Card(UUID.randomUUID(), "Fireball", "Deals fire damage to all enemies", Fireball)))

    import chousen.api.types.Implicits._

    implicit def toBattle(e: Enemy): Battle = Battle(Set(e))

    def createSlime = Battle(Set(Enemy("Slime", UUID.randomUUID(), CharStats(10, 10), 0)))

    def createSloth = Battle(Set(Enemy("Sloth", UUID.randomUUID(), CharStats(23, 23, strength = 12, speed = 4), 0)))

    def createRat = Battle(Set(Enemy("Rat", UUID.randomUUID(), CharStats(8, 8, strength = 6, speed = 12), 0)))

    def orc = battleMonoid.empty |+| Enemy("Orc", UUID.randomUUID(), CharStats(50, 50, strength = 11, vitality = 11, speed = 7), 0)

    val battle1 = createSloth
    val battle2 = createRat |+| createRat |+| createRat |+| createRat
   // val battle3 = createSlime |+| orc
    val battle4 = createSlime |+| createSloth |+| createRat
    val battle5 = orc |+| createRat |+| createSloth |+| createRat |+| createSlime

    val dungeon = Dungeon(battle1, LinearSeq(battle2, battle4, battle5))
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
    import chousen.api.types.Implicits._

    game.cards.hand
      .find(_ ~= card)
      .find(c => commandRequest match {
        case AttackRequest(_) => false
        case SelfInflictingActionRequest(action) => c.action == action
        case SingleTargetActionRequest(_, action) => c.action == action
        case MultiTargetActionRequest(_, action) => c.action == action
      })
      .fold(game)(c =>
        GameStateOptics.HandLens.modify((cs:List[Card]) => cs.filterNot(_ ~= c))
          .compose(takeCommand(commandRequest, _: GameState))
          .apply(game)
    )
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
        GameStateOptics.DungeonTriLens.modify { (pdm: (Player, Dungeon, Seq[GameMessage])) =>
          val (p, d, msgs) = pdm
          val newDungeon = Dungeon(d.remainingEncounters.head, d.remainingEncounters.tail)
          val healAmount = 20.min(p.stats.maxHp - p.stats.currentHp)

          val restMsg = GameMessage(s"${p.name} rests and recovers $healAmount hp.")
          val strongerMsg = GameMessage(s"${p.name} feels stronger after resting.")
          val progressMsg = GameMessage(s"${p.name} recklessly wanders deeper into the dungeon.")
          val encounterMsg = startEncounterMessage(newDungeon.currentEncounter.enemies, p)

          val newPlayer = PlayerCharStatsLens.composeLens(CharStatsOptics.hp).modify(_ + healAmount).compose(
          PlayerCharStatsLens.modify(cs =>
            cs.copy(strength = cs.strength + 1, dexterity = cs.dexterity + 1, vitality = cs.vitality + 1))).apply(p)

          (newPlayer, newDungeon, msgs :+ restMsg :+ strongerMsg :+ progressMsg :+ encounterMsg)
        }(game)
      } else game
    }
  }

  def startEncounterMessage(enemies: Set[Enemy], player: Player): GameMessage = {
    if (enemies.size == 1) GameMessage(s"${player.name} is attacked by ${enemies.head.name}!")
    else GameMessage(s"${player.name} is attacked by: ${enemies.toList.map(_.name).mkString(", ")}!")
  }

}
