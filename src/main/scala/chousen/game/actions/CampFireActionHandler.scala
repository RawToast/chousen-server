package chousen.game.actions

import chousen.Optics._
import chousen.api.data._
import chousen.game.cards.CardManager
import chousen.util.LensUtil
import monocle.macros.GenLens

object CampFireActionHandler extends ActionHandler {

  def handle(action: CampFireAction): (GameState) => GameState = {
    LensUtil.triLens(PlayerLens, GenLens[GameState](_.cards), MessagesLens).modify {
      case (p, cs, msgs) =>
        cardActions(action)(p, cs, msgs)
    }
  }

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
    val newCards = CardManager.fillHand(cs)
    val foundCards = diffHands(cs, newCards)

    val targetMsg = GameMessage(s"${p.name} extensively searches the area and finds: ${printCards(foundCards)}")

    val gameMessages = msgs :+ targetMsg

    (p, newCards, gameMessages)
  }


  def restAndExplore(p: Player, h: Cards, msgs: Seq[GameMessage]): (Player, Cards, Seq[GameMessage]) = {
    val maxHeal = p.stats.maxHp - p.stats.currentHp
    val healAmount: Int = Math.min(maxHeal, 10 + p.stats.vitality + (p.experience.level * 3))

    val c1 = CardManager.drawCard(h)
    val newCards = CardManager.drawCard(c1)
    val foundCards = newCards.hand.filter(c => !h.hand.contains(c))

    val msg1 = GameMessage(s"${p.name} takes a quick rest and heals $healAmount")
    val msg2 = GameMessage(s"${p.name} quickly searches the area and finds: ${printCards(foundCards)}")
    val gameMessages = msgs :+ msg1 :+ msg2

    (p, newCards, gameMessages)
  }

  private def diffHands(oldCards: Cards, newCards: Cards) =
    newCards.hand.filter(c => !oldCards.hand.contains(c))

  private def printCards(cs: Seq[Card]): String = cs.map(_.name).mkString(", ")



}
