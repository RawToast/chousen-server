package chousen.game.actions

import chousen.Optics.{MessagesLens, PlayerLens}
import chousen.api.data._
import chousen.game.cards.CardManager
import chousen.util.LensUtil
import monocle.macros.GenLens

object CardActionHandler extends ActionHandler {

  def handle(action: CardAction): (GameState) => GameState = {
    LensUtil.triLens(PlayerLens, GenLens[GameState](_.cards), MessagesLens).modify {
      case (p, cs, msgs) =>
        cardActions(action)(p, cs, msgs)
    }
  }

  private def cardActions(actionId: CardAction): (Player, Cards, Seq[GameMessage]) => (Player, Cards, Seq[GameMessage]) =
    actionId match {
      case Rummage => restore
      case Miracle => restore
      case Replace => replace
      case Restore => restore
    }


  def restore(p: Player, h: Cards, msgs: Seq[GameMessage]) = {

    val restoredCard = h.discard.head
    val remainingDiscard = h.discard.tail

    val targetMsg = GameMessage(s"${p.name} uses Restore and finds a ${restoredCard.name}")

    val gameMessages = msgs :+ targetMsg

    val newCards = Cards(h.hand :+ restoredCard, h.deck, remainingDiscard)

    (p, newCards, gameMessages)
  }

  def replace(p: Player, h: Cards, msgs: Seq[GameMessage]) = {

    val targetMsg = GameMessage(s"${p.name} uses Replace!")

    val gameMessages = msgs :+ targetMsg

    val discardedHandCards: Cards = h.hand
      .map(c => CardManager.discard(c))
        .foldLeft(h)((c, ca) => ca(c))

    @scala.annotation.tailrec
    def populate(cards: Cards): Cards = {
      if (cards.hand.size >= 7) cards
      else populate(CardManager.drawCard(discardedHandCards))
    }

    val newCards = populate(discardedHandCards)

    (p, newCards, gameMessages)
  }


}
