package chousen.game.actions

import chousen.Optics._
import chousen.api.data._
import chousen.game.cards.CardManager
import chousen.util.LensUtil
import monocle.macros.GenLens

object CampFireActionHandler extends ActionHandler {

  def handle(action: CampFireAction): (GameState) => GameState = { gs: GameState =>
    if (gs.dungeon.currentEncounter.enemies.forall(_.name != "Camp Fire")) { gs }
    else {
      LensUtil.triLens(PlayerLens, GenLens[GameState](_.cards), MessagesLens).modify {
        case (p, cs, msgs) =>
          cardActions(action)(p, cs, msgs)
      }.compose(CurrentEncounterLens.composeLens(BattleEnemiesLens).modify(es =>
       es.map(e => EnemyStatsLens.composeLens(HpLens).set(0)(e))))(gs)
    }
  }.andThen(handleDead)

  private def cardActions(actionId: CampFireAction): (Player, Cards, Seq[GameMessage]) => (Player, Cards, Seq[GameMessage]) =
    actionId match {
      case Rest => rest
      case Explore => explore
      case RestAndExplore => restAndExplore
    }


  def rest(p: Player, h: Cards, msgs: Seq[GameMessage]): (Player, Cards, Seq[GameMessage]) = {

    val healAmt = p.stats.maxHp - p.stats.currentHp

    val targetMsg = GameMessage(s"${p.name} rests and heals $healAmt")

    val gameMessages = msgs :+ targetMsg

    (PlayerHealthLens.set(p.stats.maxHp)(p), h, gameMessages)
  }

  def explore(p: Player, cs: Cards, msgs: Seq[GameMessage]): (Player, Cards, Seq[GameMessage]) = {

    val newCards = {
      val nh = CardManager.fillHand(cs)
      val handDiff = diffHands(cs, nh)

      if (handDiff.isEmpty) {
        val c1 = CardManager.drawCard(nh, CardManager.ABSOLUTE_MAX)
        val c2 = CardManager.drawCard(c1, CardManager.ABSOLUTE_MAX)
        c2
      } else if (handDiff.size == 1) {
        val c1 = CardManager.drawCard(nh, CardManager.ABSOLUTE_MAX)
        c1
      } else nh
    }


    val foundCards = diffHands(cs, newCards)

    val targetMsg = GameMessage(s"${p.name} extensively searches the area and finds: ${printCards(foundCards)}")

    val gameMessages = msgs :+ targetMsg

    (p, newCards, gameMessages)
  }


  def restAndExplore(p: Player, h: Cards, msgs: Seq[GameMessage]): (Player, Cards, Seq[GameMessage]) = {
    val maxHeal = p.stats.maxHp - p.stats.currentHp
    val healAmount: Int = Math.min(maxHeal, 10 + p.stats.vitality + (p.experience.level * 3))

    val c1 = CardManager.drawCard(h)
    val newCards = CardManager.drawCard(c1, CardManager.ABSOLUTE_MAX)
    val foundCards = newCards.hand.filter(c => !h.hand.contains(c))

    val msg1 = GameMessage(s"${p.name} takes a quick rest and heals $healAmount")
    val msg2 = GameMessage(s"${p.name} quickly searches the area and finds: ${printCards(foundCards)}")
    val gameMessages = msgs :+ msg1 :+ msg2

    (PlayerHealthLens.modify(hp => hp + healAmount)(p), newCards, gameMessages)
  }

  private def diffHands(oldCards: Cards, newCards: Cards) =
    newCards.hand.filter(c => !oldCards.hand.contains(c))

  private def printCards(cs: Seq[Card]): String = cs.map(_.name).mkString(", ")


}
