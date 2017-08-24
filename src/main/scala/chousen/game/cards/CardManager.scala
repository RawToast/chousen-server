package chousen.game.cards

import chousen.Optics._
import chousen.api.data
import chousen.api.data._
import monocle.Lens
import monocle.macros.GenLens

import scala.annotation.tailrec
import scala.util.Random

object CardManager extends CardManager {
  val initialCards: Seq[Card] = CardCatalogue.fighterDeck
}


trait CardManager {

  lazy val MAX_HAND_SIZE = 7
  lazy val PRE_DISCARD_MAX_HAND_SIZE = 8
  lazy val ABSOLUTE_MAX = 15
  lazy val essenceActions = Seq(EssenceOfStrength, EssenceOfDexterity, EssenceOfVitality, EssenceOfIntelligence)

  def playCard(card: data.Card)(f: data.Card => GameState): (GameState) => GameState = { game: GameState =>
    import chousen.Implicits._
    val action = card.action match {
      case _: CampFireAction => game.cards.passive
        .find(_ ~= card)
      case _: Action => game.cards.hand
        .find(_ ~= card)
    }

    def discard(card: Card, ng: GameState) = HandLens.modify((cs: Seq[data.Card]) => cs.filterNot(_ ~= card))
      .andThen(DiscardLens.modify((ds: Seq[data.Card]) => if (card.name.contains("Essence of")) ds else card +: ds)).apply(ng)

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
        val nextGameState = f(c)
        if (nextGameState == game) nextGameState
        else {
          //          val nextGameState = if (!essenceActions.contains(c.action)) CardsLens.modify(_.copy(playedEssence = false))(actionResult) else actionResult
          // Move to discard
          c.action match {
            case _: CampFireAction => nextGameState
            case eq: EquipAction => handleEquipAction(eq, c)(nextGameState)
            case _ =>
              c.charges match {
                case Some(i) =>
                  if (i > 1) HandLens.modify(_.map(x => if (x ~= c) c.copy(charges = c.charges.map(_ - 1)) else x))(nextGameState)
                  else discard(c, nextGameState)
                case None => discard(c, nextGameState)
              }
          }
        }
      }


  }

  def startGame(cards: Seq[data.Card], passiveCards: Seq[data.Card] = Seq.empty): Cards = {

    val (t, b) = cards.splitAt(cards.size / 2)
    val (tt, tb) = t.splitAt(t.size / 2)
    val (bt, bb) = b.splitAt(b.size / 2)

    val shuffledCards1 = Random.shuffle(Random.shuffle(bt ++ tt ++ bb ++ tb))

    val (top, bot) = shuffledCards1.splitAt(30)

    val shuffledCards = Random.shuffle(bot ++ top)

    val (hand, deck) = shuffledCards.splitAt(MAX_HAND_SIZE)

    Cards(hand, deck, Seq.empty, passiveCards, EquippedCards())
  }

  def moveCardToHand(cards: Cards, pred: Card => Boolean): Cards = {
    val optCards = for {
      ac <- cards.deck.find(pred)
      nh = cards.copy(hand = cards.hand :+ ac, deck = cards.deck.filterNot(_.id == ac.id))
    } yield nh

    optCards.getOrElse(cards)
  }

  @tailrec
  final def drawCard(cards: Cards, limit: Int = MAX_HAND_SIZE): Cards = {
    if (cards.hand.size < limit) {
      if (cards.deck.isEmpty) drawCard(cards.copy(deck = cards.discard.map(c => c.copy(charges = c.maxCharges)), discard = Seq.empty))
      else cards.copy(hand = cards.hand :+ cards.deck.head, deck = cards.deck.tail)
    } else cards
  }

  def fillHand(cards: Cards, limit: Int = MAX_HAND_SIZE): Cards = {
    @scala.annotation.tailrec
    def populate(cards: Cards): Cards = {
      if (cards.hand.size >= limit) cards
      else populate(CardManager.drawCard(cards, limit))
    }

    populate(cards)
  }

  def moveLastDiscardToTopDeck(cards: Cards): Cards = {
    cards.discard.headOption.fold(cards) { (c: data.Card) =>
      cards.copy(deck = Seq(c) ++ cards.deck, discard = cards.discard.tail)
    }
  }

  def moveCardToBottomOfDeck(card: data.Card): Cards => Cards = {
    (cards: Cards) =>
      import chousen.Implicits._
      cards.hand
        .find(_ ~= card)
        .fold(cards) { c =>
          cards.copy(hand = cards.hand.filterNot(_ ~= c), deck = cards.deck :+ c)
        }
  }


  def discard(card: data.Card): (Cards) => Cards = { cards: Cards =>
    import chousen.Implicits._
    cards.hand.find(_ ~= card).fold(cards) { c =>
      val newHand = cards.hand.filterNot(_ ~= c)
      cards.copy(hand = newHand, discard = cards.discard :+ c)
    }
  }
}
