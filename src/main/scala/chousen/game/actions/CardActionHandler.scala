package chousen.game.actions

import java.util.UUID

import chousen.Optics.{CardsLens, MessagesLens, PlayerLens}
import chousen.api.data._
import chousen.game.cards.{CardCatalogue, CardManager, Potions}
import chousen.util.CardsSyntax._
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
      case Acquire => acquire
      case Miracle => miracle
      case Replace => replace
      case Restore => restore
      case ForgeArmour => forgeArmour(cardId)
      case ForgeWeapon => forgeWeapon(cardId)
      case Trade => trade(cardId)
      case ManifestRage => manifestRage(cardId)
      case BrewPoison => brewPoison
      case MakeMiasma => makeMiasma
      case MakeAlkahest => makeAlkahest
      case EssenceBoost => essenceBoost(cardId)
      case Transmute => transmute(cardId)
      case ReduceRequirements => reduceRequirements(cardId)
      case FindersKeepers => findersKeepers(cardId)
      case AnotherTime => anotherTime(cardId)
      case Refresh => refresh
      case Armoury => armoury
      case Recharge => recharge
      case IncreaseCharges => chargeUp(cardId)
      case PurchaseTreasure => purchaseTreasure

      case BagOfGold => bagOfGold
      case PotOfGold => potOfGold
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

    val discardedHandCards: Cards =
      cs.hand.foldLeft(cs)((cs, c) => cs.discardCard(c))
//
//    val discardedHandCards: Cards = cs.hand
//      .map(c => CardManager.discard(c))
//        .foldLeft(cs)((c, ca) => ca(c))

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
    val newCards = cs.drawNoLimit.drawNoLimit.drawNoLimit

    val foundCards = newCards.hand.filter(c => !cs.hand.contains(c))

    val targetMsg = GameMessage(s"${p.name} searches the area and finds: ${foundCards.map(_.name).mkString(", ")}")

    val gameMessages = msgs :+ targetMsg

    (p, newCards, gameMessages)
  }

  def acquire(p: Player, cs: Cards, msgs: Seq[GameMessage]): (Player, Cards, Seq[GameMessage]) = {
    val newCards = cs.drawNoLimit.drawNoLimit.drawNoLimit.drawNoLimit

    val foundCards = newCards.hand.filter(c => !cs.hand.contains(c))

    val targetMsg = GameMessage(s"${p.name} uses Acquire and gains: ${foundCards.map(_.name).mkString(", ")}")

    val gameMessages = msgs :+ targetMsg

    (p, newCards, gameMessages)
  }

  def forgeArmour(cardId: Option[UUID]): (Player, Cards, Seq[GameMessage]) => (Player, Cards, Seq[GameMessage]) = {
    case (p: Player, h: Cards, msgs: Seq[GameMessage]) =>

      val newCards = for {
        id <- cardId
        discardCard <- h.hand.find(_.id == id)
        armourCard <- h.deck.find(_.action.isInstanceOf[EquipArmour])
        m = GameMessage(s"${p.name} discards ${discardCard.name} and forges: ${armourCard.name}")
        nc: Cards = h.discardCard(discardCard).moveToHand(armourCard.id)
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
        nc = h.discardCard(discardCard).moveToHand(weaponCard.id)
      } yield (nc, m)

      newCards.fold((p, h, msgs))(ncm => (p, ncm._1, msgs :+ ncm._2))
  }

  def trade(cardId: Option[UUID]): (Player, Cards, Seq[GameMessage]) => (Player, Cards, Seq[GameMessage]) = {
    case (p: Player, h: Cards, msgs: Seq[GameMessage]) =>

      val newCards = for {
        id <- cardId
        discardCard <- h.hand.find(_.id == id)

        cards = h.discardCard(discardCard).drawNoLimit.drawNoLimit.drawNoLimit

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

        handWithPotion = h.discardCard(discardCard).addToHand(potionBuilder.rage)
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
        if (c.hand.size >= CardManager.PRE_DISCARD_MAX_HAND_SIZE) c
        else addEssences(newCards)
      }

      val newCards = for {
        id <- cardId
        discardCard <- h.hand.find(_.id == id)
        discardedHand = h.discardCard(discardCard)
        cards = addEssences(discardedHand)
        foundCards = cards.hand.filter(c => !h.hand.contains(c))

        m = GameMessage(s"${p.name} uses Essence Boost and receives: ${foundCards.map(_.name).mkString(", ")}")
      } yield (cards, m)

      newCards.fold((p, h, msgs))(ncm => (p, ncm._1, msgs :+ ncm._2))
  }

  def transmute(cardId: Option[UUID]): (Player, Cards, Seq[GameMessage]) => (Player, Cards, Seq[GameMessage]) = {
    case (p: Player, h: Cards, msgs: Seq[GameMessage]) =>

      def computeGold(c: Card): Int = c.action match {
        case _: EquipAction => 60

        case _: CardAction => 30
        case _: CampFireAction => 1

        case _ => 20 + (5 * c.charges.getOrElse(1))
      }

      val newCards = for {
        id <- cardId
        discardCard <- h.hand.find(_.id == id)
        discardedHand = h.discardCard(discardCard)
        gld = computeGold(discardCard)
        m = GameMessage(s"${p.name} uses Transmute on ${discardCard.name} and gains $gld gold")
      } yield (discardedHand, m, gld)

      newCards.fold((p, h, msgs))(ncm => (p.copy(gold = p.gold + ncm._3), ncm._1, msgs :+ ncm._2))
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

    val toDiscard: Seq[Card] = cards.hand.filterNot(_.charges.nonEmpty)

    val newCards = toDiscard.foldLeft(cards)((cs, c) => cs.discardCard(c))
        .drawCard(CardManager.PRE_DISCARD_MAX_HAND_SIZE)
        .drawCard(CardManager.PRE_DISCARD_MAX_HAND_SIZE)
        .drawCard(CardManager.PRE_DISCARD_MAX_HAND_SIZE)
        .drawCard(CardManager.PRE_DISCARD_MAX_HAND_SIZE)


    val foundCards = newCards.hand.filter(c => !emptiedHand.contains(c))

    val foundMsg = GameMessage(s"${p.name} uses Refresh and finds: ${foundCards.map(_.name).mkString(", ")}")

    (p, newCards, msgs :+ foundMsg)
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

  def findersKeepers(cardId: Option[UUID]): (Player, Cards, Seq[GameMessage]) => (Player, Cards, Seq[GameMessage]) = {
    case (p: Player, cards: Cards, msgs: Seq[GameMessage]) =>

      def makeMessage(newCards: Cards, oldCards: Cards): Seq[GameMessage] = {
        newCards.hand.filter(c => !oldCards.hand.contains(c))
          .map(c => GameMessage(s"${p.name} finds ${c.name}"))

      }

      lazy val newCards: Option[(Cards, Seq[GameMessage])] = for {
        id <- cardId
        newCards = cards.moveToHand(id)
        m = makeMessage(newCards, cards)
      } yield (newCards, m)

      if (!cards.deck.find(_.id == cardId.get).forall(_.action.isInstanceOf[CardAction]))
        newCards.fold((p, cards, msgs))(ncm => (p, ncm._1, msgs ++ ncm._2))
      else (p, cards, msgs)
  }

  def anotherTime(cardId: Option[UUID]): (Player, Cards, Seq[GameMessage]) => (Player, Cards, Seq[GameMessage]) = {
    case (p: Player, cards: Cards, msgs: Seq[GameMessage]) =>

      def makeMessage(newCards: Cards, oldCards: Cards) = {
        val foundCards = newCards.hand.filter(c => !oldCards.hand.contains(c))
        GameMessage(s"${p.name} finds ${foundCards.map(_.name).mkString(", ")} once more")
      }

      val newCards: Option[(Cards, GameMessage)] = for {
        id <- cardId
        newCards = cards.moveFromDiscardToHand(id)
        m = makeMessage(newCards, cards)
      } yield (newCards, m)

      newCards.fold((p, cards, msgs))(ncm => (p, ncm._1, msgs :+ ncm._2))
  }

  def recharge(p: Player, h: Cards, msgs: Seq[GameMessage]): (Player, Cards, Seq[GameMessage]) = {
      val newHand = h.hand.map(c => c.copy(charges = c.maxCharges))
      val newSkills = h.equippedCards.skills.map(c => c.copy(charges = c.maxCharges))
      val msg = GameMessage(s"${p.name} uses Recharge")

      (p, h.copy(hand = newHand, equippedCards = h.equippedCards.copy(skills = newSkills)), msgs :+ msg)
  }

  def chargeUp(cardId: Option[UUID]): (Player, Cards, Seq[GameMessage]) => (Player, Cards, Seq[GameMessage]) = {
    case (p: Player, h: Cards, msgs: Seq[GameMessage]) =>

      val cardsAndMessages = for {
        id <- cardId
        affectedCard <- h.hand.filter(_.charges.nonEmpty).find(_.id == id)
          .fold(h.equippedCards.skills.filter(_.charges.nonEmpty).find(_.id == id))(c => Option(c))
        m = GameMessage(s"${p.name} uses Increase Charges on ${affectedCard.name}")

        rechargedCard = affectedCard
          .copy(charges = affectedCard.charges.map(_ + 1), maxCharges = affectedCard.maxCharges.map(_ + 1))
        cs = h.copy(hand = h.hand.map(c => if (c.id == rechargedCard.id) rechargedCard else c),
          equippedCards = h.equippedCards
            .copy(skills = h.equippedCards.skills.map(c => if (c.id == rechargedCard.id) rechargedCard else c)))
      } yield (cs, m)


      cardsAndMessages.fold((p, h, msgs))(ncm => (p, ncm._1, msgs :+ ncm._2))
  }

  def purchaseTreasure(p: Player, cs: Cards, msgs: Seq[GameMessage]): (Player, Cards, Seq[GameMessage]) = {
    val newCards = cs.drawTreasure

    val foundCards = newCards.hand.filter(c => !cs.hand.contains(c))

    val targetMsg = GameMessage(s"${p.name} buys ${foundCards.map(_.name).mkString(", ")}")

    val gameMessages = msgs :+ targetMsg

    (p, newCards, gameMessages)
  }



  def makeMiasma(p: Player, cs: Cards, msgs: Seq[GameMessage]): (Player, Cards, Seq[GameMessage]) = {

    def shouldMap(c: Card) = c.action == PotionOfFlames || c.action == PotionOfPoison

    val nh = cs.hand.map(c => if(shouldMap(c)) CardCatalogue.potionOfMiasma else c)

    val m = GameMessage(s"${p.name} turns their potions of Flames and Poison into Miasma")

    val gameMessages = msgs :+ m

    (p, cs.copy(hand = nh), gameMessages)
  }

  def makeAlkahest(p: Player, cs: Cards, msgs: Seq[GameMessage]): (Player, Cards, Seq[GameMessage]) = {

    def shouldMap(c: Card) = c.action == PotionOfPoison

    val nh = cs.hand.map(c => if(shouldMap(c)) CardCatalogue.potionOfAlkahest else c)

    val m = GameMessage(s"${p.name} concentrates their potions of poison into Poison into Alkahest")

    val gameMessages = msgs :+ m

    (p, cs.copy(hand = nh), gameMessages)
  }


  def brewPoison(p: Player, cs: Cards, msgs: Seq[GameMessage]): (Player, Cards, Seq[GameMessage]) = {

    val m = GameMessage(s"${p.name} makes two potions of poison")

    val gameMessages = msgs :+ m

    (p, cs.addToHand(CardCatalogue.poison).addToHand(CardCatalogue.poison), gameMessages)
  }




  def bagOfGold(p: Player, cs: Cards, msgs: Seq[GameMessage]): (Player, Cards, Seq[GameMessage]) = {

    val gold = 30
    val m = GameMessage(s"${p.name} opens a bag of gold and finds $gold gold!")

    val gameMessages = msgs :+ m

    (p.copy(gold = p.gold + gold), cs, gameMessages)
  }

  def potOfGold(p: Player, cs: Cards, msgs: Seq[GameMessage]): (Player, Cards, Seq[GameMessage]) = {

    val gold = 100
    val m = GameMessage(s"${p.name} looks in a pot of gold and finds $gold gold!")

    val gameMessages
    = msgs :+ m

    (p.copy(gold = p.gold + gold), cs, gameMessages)
  }
}
