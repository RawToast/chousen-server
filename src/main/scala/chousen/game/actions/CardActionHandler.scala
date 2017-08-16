package chousen.game.actions

import java.util.UUID

import chousen.Optics.{MessagesLens, PlayerLens}
import chousen.api.data._
import chousen.game.cards.CardManager
import chousen.util.LensUtil
import monocle.macros.GenLens

object CardActionHandler extends ActionHandler {

  def handle(action: CardAction, cardId: Option[UUID]): (GameState) => GameState = {
    LensUtil.triLens(PlayerLens, GenLens[GameState](_.cards), MessagesLens).modify {
      case (p, cs, msgs) =>
        cardActions(action, cardId)(p, cs, msgs)
    }
  }

  private def cardActions(actionId: CardAction, cardId: Option[UUID]): (Player, Cards, Seq[GameMessage]) => (Player, Cards, Seq[GameMessage]) =
    actionId match {
      case Rummage => rummage
      case Miracle => miracle
      case Replace => replace
      case Restore => restore
      case ForgeArmour => forgeArmour(cardId)
      case ForgeWeapon => forgeWeapon(cardId)
      case Trade => trade(cardId)
    }


  def restore(p: Player, h: Cards, msgs: Seq[GameMessage]): (Player, Cards, Seq[GameMessage]) = {

    val restoredCard = h.discard.head
    val remainingDiscard = h.discard.tail

    val targetMsg = GameMessage(s"${p.name} uses Restore and finds a ${restoredCard.name}")

    val gameMessages = msgs :+ targetMsg

    val newCards = h.copy(hand = h.hand :+ restoredCard, discard = remainingDiscard)

    (p, newCards, gameMessages)
  }

  def replace(p: Player, cs: Cards, msgs: Seq[GameMessage]): (Player, Cards, Seq[GameMessage]) = {
    val targetMsg = GameMessage(s"${p.name} uses Replace!")

    val gameMessages = msgs :+ targetMsg

    val discardedHandCards: Cards = cs.hand
      .map(c => CardManager.discard(c))
        .foldLeft(cs)((c, ca) => ca(c))

    val handSize = Math.max(3, cs.hand.size)

    @scala.annotation.tailrec
    def populate(cards: Cards, amount: Int): Cards = {
      if (cards.hand.size >= amount) cards
      else populate(CardManager.drawCard(cards), amount)
    }

    val newCards = populate(discardedHandCards, handSize)

    (p, newCards, gameMessages)
  }


  def miracle(p: Player, h: Cards, msgs: Seq[GameMessage]): (Player, Cards, Seq[GameMessage]) = {

    @scala.annotation.tailrec
    def populate(cards: Cards): Cards = {
      if (cards.hand.size >= CardManager.PRE_DISCARD_MAX_HAND_SIZE) cards
      else populate(CardManager.drawCard(cards, CardManager.PRE_DISCARD_MAX_HAND_SIZE))
    }
    val newCards = populate(h)
    val foundCards = newCards.hand.filter(c => !h.hand.contains(c))

    val targetMsg = GameMessage(s"${p.name} uses Miracle and finds: ${foundCards.map(_.name).mkString(", ")}")
    val gameMessages = msgs :+ targetMsg


    (p.copy(position = p.position - 100), newCards, gameMessages)
  }

  def rummage(p: Player, cs: Cards, msgs: Seq[GameMessage]): (Player, Cards, Seq[GameMessage]) = {
    val cs1 = CardManager.drawCard(cs, limit = CardManager.ABSOLUTE_MAX)
    val cs2 = CardManager.drawCard(cs1, limit = CardManager.ABSOLUTE_MAX)

    val foundCards = cs2.hand.filter(c => !cs.hand.contains(c))

    val targetMsg = GameMessage(s"${p.name} quickly searches the area and finds: ${foundCards.map(_.name).mkString(", ")}")

    val gameMessages = msgs :+ targetMsg

    (p.copy(position = p.position - 70), cs2, gameMessages)
  }


  def forgeArmour(cardId: Option[UUID]): (Player, Cards, Seq[GameMessage]) => (Player, Cards, Seq[GameMessage]) = {
    case (p: Player, h: Cards, msgs: Seq[GameMessage]) =>

      val newCards = for {
        id <- cardId
        discardCard <- h.hand.find(_.id == id)
        armourCard <- h.deck.find(_.action.isInstanceOf[EquipArmour])
        m = GameMessage(s"${p.name} discards ${discardCard.name} and forges: ${armourCard.name}")
        nh = h.hand.filterNot(_.id == id) :+ armourCard
        nc = h.copy(hand = nh, discard = h.discard :+ discardCard)
      } yield (nc, m)

      newCards.fold((p, h, msgs))(ncm => (p, ncm._1, msgs :+ ncm._2))
  }

  def forgeWeapon(cardId: Option[UUID]): (Player, Cards, Seq[GameMessage]) => (Player, Cards, Seq[GameMessage]) = {
    case (p: Player, h: Cards, msgs: Seq[GameMessage]) =>

      val newCards = for {
        id <- cardId
        discardCard <- h.hand.find(_.id == id)
        weaponCard <- h.deck.find(_.action.isInstanceOf[EquipWeapon])
        m = GameMessage(s"${p.name} discards ${discardCard.name} and forges: ${weaponCard.name}")
        nh = h.hand.filterNot(_.id == id) :+ weaponCard
        nc = h.copy(hand = nh, discard = h.discard :+ discardCard)
      } yield (nc, m)

      newCards.fold((p, h, msgs))(ncm => (p, ncm._1, msgs :+ ncm._2))
  }

  def trade(cardId: Option[UUID]): (Player, Cards, Seq[GameMessage]) => (Player, Cards, Seq[GameMessage]) = {
    case (p: Player, h: Cards, msgs: Seq[GameMessage]) =>

      val newCards = for {
        id <- cardId
        discardCard <- h.hand.find(_.id == id)

        dc = h.copy(hand = h.hand.filterNot(_.id == id), discard = h.discard :+ discardCard)
        cs1 = CardManager.drawCard(dc, limit = CardManager.PRE_DISCARD_MAX_HAND_SIZE)
        cs2 = CardManager.drawCard(cs1, limit = CardManager.PRE_DISCARD_MAX_HAND_SIZE)
        cards = CardManager.drawCard(cs2, limit = CardManager.PRE_DISCARD_MAX_HAND_SIZE)

        foundCards = cards.hand.filter(c => !h.hand.contains(c))
        m = GameMessage(s"${p.name} trades ${discardCard.name} and receives: ${foundCards.map(_.name).mkString(", ")}")
      } yield (cards, m)

      newCards.fold((p, h, msgs))(ncm => (p.copy(position = p.position - 70), ncm._1, msgs :+ ncm._2))
  }
}
