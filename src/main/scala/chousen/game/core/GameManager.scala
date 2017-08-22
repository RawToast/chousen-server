package chousen.game.core

import chousen.api.data._
import chousen.game.actions.{MultiTargetActionHandler, SelfActionHandler, SingleTargetActionHandler, _}
import chousen.game.cards.CardManager
import chousen.game.status.PostTurnStatusCalc

trait GameManager[A] {

  def takeCommand(command: CommandRequest, game: A): A

  def transition(game: A, usedCard:Boolean=false): A

  def useCard(card: Card, commandRequest: CommandRequest, game: A): A
}

class GameStateManager(damageCalculator: DamageCalculator, postStatusCalc: PostTurnStatusCalc) extends GameManager[GameState] with TurnTransition {

  val basicAttack = new BasicAttack(damageCalculator)
  val blockHandler = new BlockActionHandler()
  val equipmentActionHandler = new EquipmentActionHandler()
  val singleTargetActionHandler = new SingleTargetActionHandler(damageCalculator)
  val multiTargetActionHandler = new MultiTargetActionHandler(damageCalculator)
  val selfActionHandler = new SelfActionHandler(damageCalculator.sc)
  lazy val essenceActions = Seq(EssenceOfStrength, EssenceOfDexterity, EssenceOfVitality, EssenceOfIntelligence)

  override def useCard(card: Card, commandRequest: CommandRequest, game: GameState): GameState = {
    val p = damageCalculator.sc.calculate(game.player)

    if (p.status.map(_.effect).contains(Rage) && !card.action.isInstanceOf[CampFireAction]) {
      val msg = GameMessage(s"Cannot use ${card.name} whilst Berserk")
      game.copy(messages = game.messages :+ msg)
    } else if(essenceActions.contains(card.action) && game.cards.playedEssence) {
      val msg = GameMessage(s"Cannot use ${card.name}, as an Essence has already been played")
      game.copy(messages = game.messages :+ msg)
    } else if (p.stats.strength < card.requirements.str.getOrElse(0)
      || p.stats.dexterity < card.requirements.dex.getOrElse(0)
      || p.stats.intellect < card.requirements.int.getOrElse(0)) {
      val msg = GameMessage(
        s"Cannot use ${card.name}, ${p.name} does not meet the requirements " +
          s"(${card.requirements.str.map(i => s"Str ($i) ").getOrElse("")}" +
          s"(${card.requirements.dex.map(i => s"Dex ($i) ").getOrElse("")}" +
          s"${card.requirements.int.map(i => s"Int ($i)").getOrElse("")})")
      game.copy(messages = game.messages :+ msg)
    } else {
      CardManager.playCard(card) { (c: Card) =>
        if (commandRequest match {
          case AttackRequest(_) => false
          case BlockRequest() => false
          case SelfInflictingActionRequest(action) => c.action == action
          case SingleTargetActionRequest(_, action) => c.action == action
          case MultiTargetActionRequest(_, action) => c.action == action
          case CardActionRequest(action, id) =>
            val isCorrectAction = c.action == action
            val inHand = id.fold(true)(uuid => game.cards.hand.exists(_.id == uuid))
            isCorrectAction && inHand
          case CampfireActionRequest(action, id) =>
            val isCorrectAction = c.action == action
            val inHand = id.fold(true)(uuid => game.cards.hand.exists(_.id == uuid))
            isCorrectAction && inHand
          case EquipmentActionRequest(_, action) => c.action == action
        }) takeCommand(commandRequest, game)
        else game
      }.apply(game)
    }
  }

  override def transition(game: GameState, usedCard: Boolean): GameState =
    transitionGame(game, postStatusCalc, usedCard)

  override def takeCommand(command: CommandRequest, game: GameState): GameState = {
    val newState = command match {
      case AttackRequest(targetId) =>
        val newState = GameTurnLoop.takeTurn(game, basicAttack.attack(targetId))
        transition(newState)
      case BlockRequest() =>
        val newState = GameTurnLoop.takeTurn(game, blockHandler.block())
        transition(newState)
      case SelfInflictingActionRequest(a) =>
        val resetEssences = !essenceActions.contains(a)
        val ns = GameTurnLoop.takeTurn(game,
          selfActionHandler.handle(a).apply, resetEssence = resetEssences)
          transition(ns, usedCard = true)
      case SingleTargetActionRequest(targetId, action) =>
        val ns= GameTurnLoop.takeTurn(game,
          singleTargetActionHandler.handle(targetId, action))
        transition(ns, usedCard = true)
      case MultiTargetActionRequest(targets, action) =>
        val ns = GameTurnLoop.takeTurn(game,
          multiTargetActionHandler.handle(targets, action))
        transition(ns, usedCard = true)
      case CardActionRequest(action, id) =>
        val ns = GameTurnLoop.takeTurn(game, CardActionHandler.handle(action, id))
        transition(ns, usedCard = true)
      case CampfireActionRequest(action, cardId) =>
        val ns = GameTurnLoop.takeTurn(game, CampFireActionHandler.handle(action, cardId))
        transition(ns)
      case EquipmentActionRequest(id, action) =>
        val ns = GameTurnLoop.takeTurn(game, equipmentActionHandler.handle(action, id))
        transition(ns)
    }

    newState
  }

}


trait TurnTransition {
  import chousen.Optics._

  def transitionGame(game: GameState, statusCalc: PostTurnStatusCalc, usedCard: Boolean = false): GameState = {
    val playerIsDead = game.player.stats.currentHp <= 0
    lazy val deathMessage = GameMessage(s"${game.player.name} dies.")
    lazy val winMessage = GameMessage(s"A winner is ${game.player.name}!")

    if (playerIsDead) MessagesLens.modify(msgs => msgs :+ deathMessage)(game)
    else {
      if (game.dungeon.currentEncounter.enemies.isEmpty && game.dungeon.remainingEncounters.isEmpty)
        MessagesLens.modify(msgs => msgs :+ winMessage)(game)
      else if (game.dungeon.currentEncounter.enemies.isEmpty && game.dungeon.remainingEncounters.nonEmpty) {
        postBattle(game, statusCalc, usedCard)
      } else statusCalc.applyStatusEffects(game)
    }
  }

  private def postBattle(gs: GameState, statusCalc: PostTurnStatusCalc, playedCard: Boolean): GameState = {

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
      .andThen(PlayerLens.composeLens(PlayerStatusLens).modify(statusCalc.reduceStatusLength)).apply(gs)

    val drawLimit = if (playedCard) CardManager.PRE_DISCARD_MAX_HAND_SIZE else CardManager.MAX_HAND_SIZE

    g.copy(cards = CardManager.drawCard(CardManager.drawCard(g.cards, drawLimit), drawLimit))
  }


  private def startEncounterMessage(enemies: Set[Enemy], player: Player): GameMessage = {
    if (enemies.size == 1 && enemies.head.stats.speed == 0) GameMessage(s"${player.name} discovers a ${enemies.head.name}.")
    else if (enemies.size == 1) GameMessage(s"${player.name} is attacked by ${enemies.head.name}!")
    else GameMessage(s"${player.name} is attacked by: ${enemies.toList.map(_.name).mkString(", ")}!")
  }
}
