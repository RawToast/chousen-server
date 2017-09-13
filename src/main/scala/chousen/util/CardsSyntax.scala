package chousen.util

import java.util.UUID

import chousen.api.data.{Card, Cards}
import chousen.game.cards.CardManager

object CardsSyntax extends CardsSyntax

trait CardsSyntax {
  //
  implicit class CmSyntax(cs: Cards) {
    def discardCard(c: Card) = {
      CardManager.discard(c)(cs)
    }

    def addToHand(c: Card) = CardManager.addCard(c)(cs)

    def moveToHand(pred : Card => Boolean) = CardManager.moveCardToHand(cs, pred)
    def moveToHand(id : UUID) = CardManager.moveCardToHand(cs, c => c.id == id)

    def moveFromDiscardToHand(id : UUID) = CardManager.moveCardFromDiscardToHand(cs, c => c.id == id)


    def drawCard(limit: Int=CardManager.MAX_HAND_SIZE) = CardManager.drawCard(cs, limit)
    def drawNoLimit = CardManager.drawCard(cs, CardManager.ABSOLUTE_MAX)

    def drawTreasure = CardManager.drawTreasure(cs)
  }
}
