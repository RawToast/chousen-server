package chousen.game.actions

import java.util.UUID

import chousen.Optics._
import chousen.api.data._
import chousen.game.cards.CardManager
import chousen.util.CardsSyntax._
import chousen.util.LensUtil
import monocle.Lens
import monocle.macros.GenLens

object CampFireActionHandler extends ActionHandler {

  def handle(action: CampFireAction, cardId: Option[UUID]): (GameState) => GameState = { gs: GameState =>
    if (gs.dungeon.currentEncounter.enemies.forall(_.name != "Camp Fire")) {
      gs
    }
    else {
      val hp = action match {
        case (Drop | Destroy | LearnSkill) => 3
        case _ => 0
      }
      LensUtil.triLens(PlayerLens, GenLens[GameState](_.cards), MessagesLens).modify {
        case (p, cs, msgs) =>
          cardActions(action, cardId)(p, cs, msgs)
      }.compose(CurrentEncounterLens.composeLens(BattleEnemiesLens).modify(es =>
        es.map(e => EnemyStatsLens.composeLens(HpLens).set(hp)(e))))(gs)
    }
  }.andThen(handleDead)

  private def cardActions(actionId: CampFireAction, cardId: Option[UUID]): (Player, Cards, Seq[GameMessage]) => (Player, Cards, Seq[GameMessage]) =
    actionId match {
      case Rest => rest
      case Explore => explore
      case RestAndExplore => restAndExplore
      case Drop => drop(cardId)
      case Destroy => destroy(cardId)
      case LearnSkill => learn(cardId)
    }


  def drop(cardId: Option[UUID]): (Player, Cards, Seq[GameMessage]) => (Player, Cards, Seq[GameMessage]) = {
    case (p: Player, h: Cards, msgs: Seq[GameMessage]) =>
      val newCards = for {
        id <- cardId
        discardCard <- h.hand.find(_.id == id)
        m = GameMessage(s"${p.name} drops ${discardCard.name} by the camp fire")
        nc = h.discardCard(discardCard)
      } yield (nc, m)

      newCards.fold((p, h, msgs))(ncm => (p, ncm._1, msgs :+ ncm._2))
  }

  def destroy(cardId: Option[UUID]): (Player, Cards, Seq[GameMessage]) => (Player, Cards, Seq[GameMessage]) = {
    case (p: Player, h: Cards, msgs: Seq[GameMessage]) =>

      val newCards = for {
        id <- cardId
        destroyedCard <- h.hand.find(_.id == id)
        msg = GameMessage(s"${p.name} destroys ${destroyedCard.name} in the fire")
        newHand = h.hand.filterNot(_.id == id)
        newCards = h.copy(hand = newHand)
      } yield (newCards, msg)

      newCards.fold((p, h, msgs))(ncm => (p, ncm._1, msgs :+ ncm._2))
  }

  def rest(p: Player, h: Cards, msgs: Seq[GameMessage]): (Player, Cards, Seq[GameMessage]) = {

    val healAmt = p.stats.maxHp - p.stats.currentHp

    val targetMsg = GameMessage(s"${p.name} rests and heals $healAmt")
    val refreshedCards = SkillsLens.modify(_.map(c => c.copy(charges = c.maxCharges)))(h)

    val gameMessages = msgs :+ targetMsg

    (PlayerHealthLens.set(p.stats.maxHp)(p), refreshedCards, gameMessages)
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

    val refreshedCards = SkillsLens.modify(_.map(c => c.copy(charges = c.maxCharges)))(newCards)

    val targetMsg = GameMessage(s"${p.name} extensively searches the area and finds: ${printCards(foundCards)}")

    val gameMessages = msgs :+ targetMsg

    (p, refreshedCards, gameMessages)
  }


  def restAndExplore(p: Player, h: Cards, msgs: Seq[GameMessage]): (Player, Cards, Seq[GameMessage]) = {
    val maxHeal = p.stats.maxHp - p.stats.currentHp
    val healAmount: Int = Math.min(maxHeal, 10 + p.stats.vitality + (p.experience.level * 3))

    val c1 = CardManager.drawCard(h)
    val newCards = CardManager.drawCard(c1, CardManager.ABSOLUTE_MAX)

    val refreshedCards = SkillsLens.modify(_.map(c => c.copy(charges = c.maxCharges)))(newCards)

    val foundCards = newCards.hand.filter(c => !h.hand.contains(c))

    val msg1 = GameMessage(s"${p.name} takes a quick rest and heals $healAmount")
    val msg2 = GameMessage(s"${p.name} quickly searches the area and finds: ${printCards(foundCards)}")
    val gameMessages = msgs :+ msg1 :+ msg2

    (PlayerHealthLens.modify(hp => hp + healAmount)(p), refreshedCards, gameMessages)
  }

  def learn(cardId: Option[UUID]): (Player, Cards, Seq[GameMessage]) => (Player, Cards, Seq[GameMessage]) = {
    case (p: Player, cards: Cards, msgs: Seq[GameMessage]) =>
      val pInt = p.stats.intellect

      if (pInt / 10 >= cards.equippedCards.skills.size) {
        val newCards = for {
          id <- cardId
          cardToLearn <- cards.hand.filter(_.charges.nonEmpty).find(_.id == id)
          movedCardHand = SkillsLens.modify(_ :+ cardToLearn)(cards)

          newHand = movedCardHand.hand.filterNot(_.id == id)
          newCards = movedCardHand.copy(hand = newHand)

          msg = GameMessage(s"${p.name} learns ${cardToLearn.name}!")
        } yield (newCards, msg)

        newCards.fold((p, cards, msgs))(ncm => (p, ncm._1, msgs :+ ncm._2))
      } else (p, cards,
        msgs :+ GameMessage(s"${p.name} does not have the intellect (${pInt + 10 - (pInt % 10)}) to learn another skill"))
  }

  lazy val SkillsLens: Lens[Cards, Seq[Card]] = GenLens[Cards](_.equippedCards.skills)

  private def diffHands(oldCards: Cards, newCards: Cards) =
    newCards.hand.filter(c => !oldCards.hand.contains(c))

  private def printCards(cs: Seq[Card]): String = cs.map(_.name).mkString(", ")


}
