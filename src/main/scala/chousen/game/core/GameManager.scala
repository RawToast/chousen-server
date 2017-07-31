package chousen.game.core

import chousen.api.data._
import chousen.game.actions.{MultiTargetActionHandler, SelfActionHandler, SingleTargetActionHandler, _}
import chousen.game.cards.CardManager

trait GameManager[A] {

  def takeCommand(command: CommandRequest, game: A): A

  def transition(game: A, usedCard:Boolean=false): A

  def useCard(card: Card, commandRequest: CommandRequest, game: A): A
}

class GameStateManager(damageCalculator: DamageCalculator) extends GameManager[GameState] with TurnTransition {

  val basicAttack = new BasicAttack(damageCalculator)
  val singleTargetActionHandler = new SingleTargetActionHandler(damageCalculator.sc)
  val multiTargetActionHandler = new MultiTargetActionHandler(damageCalculator.sc)
  val selfActionHandler = new SelfActionHandler(damageCalculator.sc)

  override def useCard(card: Card, commandRequest: CommandRequest, game: GameState): GameState = {

    CardManager.playCard(card) { (c: Card) =>
      if (commandRequest match {
        case AttackRequest(_) => false
        case SelfInflictingActionRequest(action) => c.action == action
        case SingleTargetActionRequest(_, action) => c.action == action
        case MultiTargetActionRequest(_, action) => c.action == action
        case CardActionRequest(action) => c.action == action
        case CampfireActionRequest(action) => c.action == action
      }) takeCommand(commandRequest, game)
      else game
    }.apply(game)
  }

  override def takeCommand(command: CommandRequest, game: GameState): GameState = {
    val newState = command match {
      case AttackRequest(targetId) =>
        val newState = GameTurnLoop.takeTurn(game, basicAttack.attack(targetId))
        transition(newState)
      case SelfInflictingActionRequest(a) =>
        val ns = GameTurnLoop.takeTurn(game,
          selfActionHandler.handle(a).apply)
          transition(ns, usedCard = true)
      case SingleTargetActionRequest(targetId, action) =>
        val ns= GameTurnLoop.takeTurn(game,
          singleTargetActionHandler.handle(targetId, action))
        transition(ns, usedCard = true)
      case MultiTargetActionRequest(targets, action) =>
        val ns = GameTurnLoop.takeTurn(game,
          multiTargetActionHandler.handle(targets, action))
        transition(ns, usedCard = true)
      case CardActionRequest(action) =>
        val ns = GameTurnLoop.takeTurn(game, CardActionHandler.handle(action))
        transition(ns, usedCard = true)
      case CampfireActionRequest(action) =>
        val ns = GameTurnLoop.takeTurn(game, CampFireActionHandler.handle(action))
        transition(ns)
    }

    newState
  }

}


trait TurnTransition {
  import chousen.Optics._

  def transition(game: GameState, usedCard:Boolean = false): GameState = {
    val playerIsDead = game.player.stats.currentHp <= 0
    lazy val deathMessage = GameMessage(s"${game.player.name} dies.")
    lazy val winMessage = GameMessage(s"A winner is ${game.player.name}!")

    if (playerIsDead) MessagesLens.modify(msgs => msgs :+ deathMessage)(game)
    else {
      if (game.dungeon.currentEncounter.enemies.isEmpty && game.dungeon.remainingEncounters.isEmpty)
        MessagesLens.modify(msgs => msgs :+ winMessage)(game)
      else if (game.dungeon.currentEncounter.enemies.isEmpty && game.dungeon.remainingEncounters.nonEmpty) {
        postTurn(game, usedCard)
      } else PlayerLens.composeLens(PlayerStatusLens).modify(handleStatus)(game)
    }
  }

  private def postTurn(gs:GameState, playedCard: Boolean): GameState = {

    val g = DungeonTriLens.modify { (pdm: (Player, Dungeon, Seq[GameMessage])) =>
      val (p, d, msgs) = pdm
      val newDungeon = Dungeon(d.remainingEncounters.head, d.remainingEncounters.tail)
      val healAmount = Math.min(Math.max(0, p.stats.maxHp - p.stats.currentHp), 20)

      val restMsg = GameMessage(s"${p.name} rests and recovers $healAmount hp.")
      val progressMsg = GameMessage(s"${p.name} recklessly wanders deeper into the dungeon.")
      val encounterMsg = startEncounterMessage(newDungeon.currentEncounter.enemies, p)

      val newPlayer = PlayerCharStatsLens.composeLens(HpLens).modify(_ + healAmount).apply(p)
      (newPlayer, newDungeon, msgs :+ restMsg :+ progressMsg :+ encounterMsg)
    }.andThen(EncounterLens.modify(GameOps.updateUntilPlayerIsActive))
      .andThen(PlayerLens.composeLens(PlayerStatusLens).modify(handleStatus)).apply(gs)

    val drawLimit = if(playedCard) CardManager.PRE_DISCARD_MAX_HAND_SIZE else CardManager.MAX_HAND_SIZE

    g.copy(cards = CardManager.drawCard(CardManager.drawCard(g.cards, drawLimit), drawLimit))
  }


  private def handleStatus(status: Seq[Status]): Seq[Status] =
    status.filter(_.turns != 0)
      .map(s => s.copy(turns = s.turns - 1))


  private def startEncounterMessage(enemies: Set[Enemy], player: Player): GameMessage = {
    if (enemies.size == 1) GameMessage(s"${player.name} is attacked by ${enemies.head.name}!")
    else GameMessage(s"${player.name} is attacked by: ${enemies.toList.map(_.name).mkString(", ")}!")
  }
}
