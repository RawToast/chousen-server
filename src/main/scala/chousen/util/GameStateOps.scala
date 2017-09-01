package chousen.util

import java.util.UUID

import chousen.api.data._


trait GameStateOps {

  def toGameResponse(gs: GameState): GameResponse = {

    def mkChargesStr(c: Card) = for {
      charges <- c.charges
      max <- c.maxCharges
    } yield s"($charges/$max)"

    def canPlayCard(c :Card):Boolean = if (c.name.contains("Essence")) {
      if (gs.cards.playedEssence) false else true
    } else true

    def toCardResponse(c: Card) = CardResponse(c.name, c.description, mkChargesStr(c), canPlayCard(c))

    val newHand = gs.cards.hand.map(toCardResponse)
    val newDeck = gs.cards.deck.map(toCardResponse)
    val newDiscard = gs.cards.discard.map(toCardResponse)
    val newPassives = gs.cards.passive.map(toCardResponse)

    val weaponResp = gs.cards.equippedCards.weapon.map(toCardResponse)
    val armourResp = gs.cards.equippedCards.armour.map(toCardResponse)
    val equipResp = EquippedCardsResponse(weaponResp, armourResp)

    val cards = CardsResponse(newHand, newDeck, newDiscard, newPassives, equipResp)
    GameResponse(gs.uuid, gs.player, cards, gs.dungeon.currentEncounter, gs.messages)
  }


  implicit class ToCardResponse(gs: GameState){
    def asResponse: GameResponse = toGameResponse(gs)
  }
}

case class GameResponse(uuid: UUID, player: Player, cards: CardsResponse, currentEncounter: Battle, messages: Seq[GameMessage])

case class CardsResponse(hand: Seq[CardResponse], deck: Seq[CardResponse], discard: Seq[CardResponse],
                         passive: Seq[CardResponse], equippedCards: EquippedCardsResponse)
case class EquippedCardsResponse(weapon: Option[CardResponse]=None, armour: Option[CardResponse]=None, jewelery: Option[CardResponse]=None)

case class CardResponse(name: String, description: String, charges: Option[String], playable:Boolean)