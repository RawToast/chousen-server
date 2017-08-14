package chousen.game.cards
import chousen.Optics._
import chousen.api.data
import chousen.api.data._
import monocle.Lens
import monocle.macros.GenLens

import scala.annotation.tailrec
import scala.util.Random

object CardManager extends CardManager {
  val initialCards: Seq[Card] = CardCatalogue.defaultDeck
}


trait CardManager {

  lazy val MAX_HAND_SIZE = 7
  lazy val PRE_DISCARD_MAX_HAND_SIZE = 8
  lazy val ABSOLUTE_MAX = 15

  def playCard(card: data.Card)(f: data.Card => GameState): (GameState) => GameState = { game: GameState =>
    import chousen.Implicits._
    val action = card.action match {
      case _: CampFireAction => game.cards.passive
        .find(_ ~= card)
      case _: Action => game.cards.hand
        .find(_ ~= card)
    }

    def discard(card: Card, ng: GameState) =  HandLens.modify((cs: Seq[data.Card]) => cs.filterNot(_ ~= card))
      .andThen(DiscardLens.modify((ds: Seq[data.Card]) => card +: ds)).apply(ng)

    def handleEquipAction(ea: EquipAction, c: Card): (GameState) => GameState = {
      def equip(optCard: Option[Card], lens: Lens[EquippedCards, Option[Card]]): (GameState) => GameState = {
        optCard match {
          case Some(k) =>
            EquipmentLens.composeLens(lens).set(Option(c))
              .andThen(HandLens.modify((cs: Seq[data.Card]) => cs.filterNot(_ ~= c)))
              .andThen(HandLens.modify(_ :+ k))
          case None =>
            EquipmentLens.composeLens(lens).set(Option(c)).andThen(
              HandLens.modify((cs: Seq[data.Card]) => cs.filterNot(_ ~= c)))
        }

      }

      ea match {
        case _: EquipWeapon => equip(game.cards.equippedCards.weapon, GenLens[EquippedCards](_.weapon))
        case _: EquipArmour => equip(game.cards.equippedCards.armour, GenLens[EquippedCards](_.armour))
      }
    }

    action
      .fold(game) { c =>
        val ng = f(c)
        if (ng == game) ng
        else {
          // Move to discard
          c.action match {
            case _: CampFireAction => ng
            case eq: EquipAction => handleEquipAction(eq, c)(ng)
            case _ =>
              c.charges match {
                case Some(i) =>
                  if (i > 1) HandLens.modify(_.map(x => if(x ~= c) c.copy(charges = c.charges.map(_ - 1)) else x))(ng)
                  else discard(c, ng)
                case None => discard(c, ng)
              }
          }
        }
      }


  }

  def startGame(cards:Seq[data.Card], passiveCards: Seq[data.Card]=Seq.empty): Cards = {
    val shuffledCards = Random.shuffle(Random.shuffle(cards))

    val (hand, deck) = shuffledCards.splitAt(MAX_HAND_SIZE)

    Cards(hand, deck, Seq.empty, passiveCards, EquippedCards())
  }

  @tailrec
  final def drawCard(cards: Cards, limit:Int=MAX_HAND_SIZE): Cards = {
    if (cards.hand.size < limit) {
      if (cards.deck.isEmpty) drawCard(cards.copy(deck = cards.discard.map(c => c.copy(charges = c.maxCharges)), discard = Seq.empty))
        else cards.copy(hand = cards.hand :+ cards.deck.head, deck = cards.deck.tail)
    } else cards
  }

  def fillHand(cards: Cards, limit: Int= MAX_HAND_SIZE): Cards = {
    @scala.annotation.tailrec
    def populate(cards: Cards): Cards = {
      if (cards.hand.size >= limit) cards
      else populate(CardManager.drawCard(cards, limit))
    }
    populate(cards)
  }

  def moveLastDiscardToTopDeck(cards: Cards): Cards = {
    cards.discard.headOption.fold(cards){ (c: data.Card) =>
      cards.copy(deck = Seq(c) ++ cards.deck, discard=cards.discard.tail)
    }
  }

  def moveCardToBottomOfDeck(card: data.Card): Cards => Cards = {
    (cards:Cards) =>
      import chousen.Implicits._
      cards.hand
        .find(_ ~= card)
        .fold(cards){ c =>
          cards.copy(hand = cards.hand.filterNot(_ ~= c), deck=cards.deck :+ c)
        }
  }


  def discard(card: data.Card): (Cards) => Cards = { cards: Cards =>
    import chousen.Implicits._
    cards.hand.find(_ ~= card).fold(cards) { c =>
      val newHand = cards.hand.filterNot(_ ~= c)
      cards.copy(hand=newHand, discard= cards.discard :+ c)
    }
  }
}
