package chousen.game.actions

import java.util.UUID

import chousen.Optics.{CardsLens, MessagesLens, PlayerLens}
import chousen.api.data._
import chousen.game.cards.{CardManager, Potions}
import chousen.util.LensUtil

import scala.util.Random

object CardActionHandler extends ActionHandler {

  def handle(action: CardAction, cardId: Option[UUID]): (GameState) => GameState = {
    LensUtil.triLens(PlayerLens, CardsLens, MessagesLens).modify {
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
      case ManifestRage => manifestRage(cardId)
      case EssenceBoost => essenceBoost(cardId)
      case ReduceRequirements => reduceRequirements(cardId)
      case Refresh => refresh
      case Armoury => armoury
      case Recharge => recharge(cardId)
      case IncreaseCharges => chargeUp(cardId)
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


    (p, newCards, gameMessages)
  }

  def rummage(p: Player, cs: Cards, msgs: Seq[GameMessage]): (Player, Cards, Seq[GameMessage]) = {
    val cs1 = CardManager.drawCard(cs, limit = CardManager.ABSOLUTE_MAX)
    val cs2 = CardManager.drawCard(cs1, limit = CardManager.ABSOLUTE_MAX)

    val foundCards = cs2.hand.filter(c => !cs.hand.contains(c))

    val targetMsg = GameMessage(s"${p.name} quickly searches the area and finds: ${foundCards.map(_.name).mkString(", ")}")

    val gameMessages = msgs :+ targetMsg

    (p, cs2, gameMessages)
  }


  def forgeArmour(cardId: Option[UUID]): (Player, Cards, Seq[GameMessage]) => (Player, Cards, Seq[GameMessage]) = {
    case (p: Player, h: Cards, msgs: Seq[GameMessage]) =>

      val newCards = for {
        id <- cardId
        discardCard <- h.hand.find(_.id == id)
        armourCard <- h.deck.find(_.action.isInstanceOf[EquipArmour])
        m = GameMessage(s"${p.name} discards ${discardCard.name} and forges: ${armourCard.name}")
        nh = h.hand.filterNot(_.id == id) :+ armourCard

        nc = h.copy(hand = nh,
          deck = h.deck.filterNot(_.id == armourCard.id),
          discard = h.discard :+ discardCard)
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
        nc = h.copy(hand = nh,
          deck = h.deck.filterNot(_.id == weaponCard.id),
          discard = h.discard :+ discardCard)
      } yield (nc, m)

      newCards.fold((p, h, msgs))(ncm => (p, ncm._1, msgs :+ ncm._2))
  }

  def trade(cardId: Option[UUID]): (Player, Cards, Seq[GameMessage]) => (Player, Cards, Seq[GameMessage]) = {
    case (p: Player, h: Cards, msgs: Seq[GameMessage]) =>

      val newCards = for {
        id <- cardId
        discardCard <- h.hand.find(_.id == id)

        dc = h.copy(hand = h.hand.filterNot(_.id == id), discard = h.discard :+ discardCard)
        cs1 = CardManager.drawCard(dc, limit = CardManager.ABSOLUTE_MAX)
        cs2 = CardManager.drawCard(cs1, limit = CardManager.ABSOLUTE_MAX)
        cards = CardManager.drawCard(cs2, limit = CardManager.ABSOLUTE_MAX)

        foundCards = cards.hand.filter(c => !h.hand.contains(c))
        m = GameMessage(s"${p.name} trades ${discardCard.name} and receives: ${foundCards.map(_.name).mkString(", ")}")
      } yield (cards, m)

      newCards.fold((p, h, msgs))(ncm => (p, ncm._1, msgs :+ ncm._2))
  }


  def manifestRage(cardId: Option[UUID]): (Player, Cards, Seq[GameMessage]) => (Player, Cards, Seq[GameMessage]) = {
    case (p: Player, h: Cards, msgs: Seq[GameMessage]) =>

      val potionBuilder = new Potions{}

      val newCards = for {
        id <- cardId
        discardCard <- h.hand.find(_.id == id)

        discardedHand = h.copy(hand = h.hand.filterNot(_.id == id), discard = h.discard :+ discardCard)
        handWithPotion = discardedHand.copy(hand = discardedHand.hand :+ potionBuilder.rage)
        cards = handWithPotion.copy(deck = Random.shuffle(h.deck :+ potionBuilder.rage))

        m = GameMessage(s"${p.name} uses Manifest Rage! ${p.name} finds a Potion of Rage. " +
          s"An additional Potion of Rage has been added to your deck.")
      } yield (cards, m)

      newCards.fold((p, h, msgs))(ncm => (p, ncm._1, msgs :+ ncm._2))
  }

  def essenceBoost(cardId: Option[UUID]): (Player, Cards, Seq[GameMessage]) => (Player, Cards, Seq[GameMessage]) = {
    case (p: Player, h: Cards, msgs: Seq[GameMessage]) =>

      @scala.annotation.tailrec
      def addEssences(c: Cards): Cards = {
        lazy val newCards = CardManager.moveCardToHand(c, _.name.contains("Essence of"))
        if (c.hand.size >= CardManager.MAX_HAND_SIZE) c
        else addEssences(newCards)
      }

      val newCards = for {
        id <- cardId
        discardCard <- h.hand.find(_.id == id)
        discardedHand = h.copy(hand = h.hand.filterNot(_.id == id), discard = h.discard :+ discardCard)
        cards = addEssences(discardedHand)
        foundCards = cards.hand.filter(c => !h.hand.contains(c))

        m = GameMessage(s"${p.name} uses Essence Boost and receives: ${foundCards.map(_.name).mkString(", ")}")
      } yield (cards, m)

      newCards.fold((p, h, msgs))(ncm => (p, ncm._1, msgs :+ ncm._2))
  }

  def armoury(p: Player, cards: Cards, msgs: Seq[GameMessage]): (Player, Cards, Seq[GameMessage]) = {

    @scala.annotation.tailrec
    def placeEquippablesOnTop(c: Cards, ecs: Seq[Card]=Seq.empty): Cards = {
      val equippableCards = c.deck.filter(_.action.isInstanceOf[EquipAction])
      if (equippableCards.isEmpty || ecs.size >= 2) c.copy(deck = ecs ++ c.deck)
      else {
        val ec = equippableCards.head
        val nc = c.copy(deck = c.deck.filterNot(_.id == ec.id))
        placeEquippablesOnTop(nc, ecs :+ ec)
      }
    }

    val newCards = placeEquippablesOnTop(cards)
    val foundCards = newCards.deck.takeWhile(_.id != cards.deck.head.id)

    val foundMsg = GameMessage(s"${p.name} uses Armoury and places: ${foundCards.map(_.name).mkString(", ")} to the top" +
      s" of the deck")

    (p, newCards, msgs :+ foundMsg)
  }

  def refresh(p: Player, cards: Cards, msgs: Seq[GameMessage]): (Player, Cards, Seq[GameMessage]) = {
    val emptiedHand = cards.hand.filter(_.charges.nonEmpty)

    val toDiscard = cards.hand.filterNot(_.charges.nonEmpty)

    val nc = cards.copy(hand = emptiedHand, discard = cards.discard ++ toDiscard)

    val d1 = CardManager.drawCard(nc, limit = CardManager.PRE_DISCARD_MAX_HAND_SIZE)
    val d2= CardManager.drawCard(d1, limit = CardManager.PRE_DISCARD_MAX_HAND_SIZE)
    val d3 = CardManager.drawCard(d2, limit = CardManager.PRE_DISCARD_MAX_HAND_SIZE)
    val d4 = CardManager.drawCard(d3, limit = CardManager.PRE_DISCARD_MAX_HAND_SIZE)

    val foundCards = d4.hand.filter(c => !emptiedHand.contains(c))

    val foundMsg = GameMessage(s"${p.name} uses Refresh and finds: ${foundCards.map(_.name).mkString(", ")}")

    (p, d4, msgs :+ foundMsg)
  }


  def reduceRequirements(cardId: Option[UUID]): (Player, Cards, Seq[GameMessage]) => (Player, Cards, Seq[GameMessage]) = {
    case (p: Player, h: Cards, msgs: Seq[GameMessage]) =>

      def reduceRequirements(r: Requirements) = {
        val sr = r.str.map(i => Math.max(1, i - 5))
        val dr = r.dex.map(i => Math.max(1, i - 5))
        val ir = r.int.map(i => Math.max(1, i - 5))

        Requirements(sr, dr, ir)
      }

      val newCards: Option[(Cards, GameMessage)] = for {
        id <- cardId
        affectedCard <- h.hand.find(_.id == id)
        updatedCard = affectedCard.copy(requirements = reduceRequirements(affectedCard.requirements))
        m = GameMessage(s"${p.name} uses Reduce Requirements on ${affectedCard.name}")
        cs = h.copy(hand = h.hand.map(c => if (c.id == affectedCard.id) updatedCard else c))
      } yield (cs, m)

      newCards.fold((p, h, msgs))(ncm => (p, ncm._1, msgs :+ ncm._2))
  }

  def recharge(cardId: Option[UUID]): (Player, Cards, Seq[GameMessage]) => (Player, Cards, Seq[GameMessage]) = {
    case (p: Player, h: Cards, msgs: Seq[GameMessage]) =>

      val cardsAndMessages = for {
        id <- cardId
        affectedCard <- h.hand.find(_.id == id)
        m = GameMessage(s"${p.name} uses Recharge on ${affectedCard.name}")

        rechargedCard = affectedCard.copy(charges = affectedCard.maxCharges)
        cs = h.copy(hand = h.hand.map(c => if (c.id == rechargedCard.id) rechargedCard else c))
      } yield (cs, m)


      cardsAndMessages.fold((p, h, msgs))(ncm => (p, ncm._1, msgs :+ ncm._2))
  }

  def chargeUp(cardId: Option[UUID]): (Player, Cards, Seq[GameMessage]) => (Player, Cards, Seq[GameMessage]) = {
    case (p: Player, h: Cards, msgs: Seq[GameMessage]) =>

      val cardsAndMessages = for {
        id <- cardId
        affectedCard <- h.hand.find(_.id == id).filter(_.charges.nonEmpty)
        m = GameMessage(s"${p.name} uses Increase Charges on ${affectedCard.name}")

        rechargedCard = affectedCard.copy(charges = affectedCard.charges.map(_ + 2), maxCharges = affectedCard.maxCharges.map(_ + 2))
        cs = h.copy(hand = h.hand.map(c => if (c.id == rechargedCard.id) rechargedCard else c))
      } yield (cs, m)


      cardsAndMessages.fold((p, h, msgs))(ncm => (p, ncm._1, msgs :+ ncm._2))
  }
}
