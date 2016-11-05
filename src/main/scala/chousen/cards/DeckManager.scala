package chousen.cards

import cats.data.Xor

//TODO the dm is passed around, but not used
case class DeckManager(hand: Hand, deck: Deck) {

  def startGame: DeckManager = (DeckManager.apply _).tupled(this.deck.shuffle.deal)

  // Consider Xor response
  def discard(cardToDiscard: Card): DeckManager = {
    DeckManager(
      hand.discard(cardToDiscard),
      deck.discard(cardToDiscard))
  }

  /**
    * Returns right hand side if successful.
    * Returns left hand side if no cards are left to draw.
    */
  def drawCard: Xor[DeckManager, DeckManager] = {
    deck.draw match {
      case Xor.Left(d: Deck) => Xor.Left(this)
      case Xor.Right((c: Card, d: Deck)) => Xor.Right(DeckManager(hand + c, d))
    }
  }

  /**
    * Returns right hand side if successful.
    * Returns left hand side if no cards are in the discard pile.
    */
  def populateFromDiscard: Xor[DeckManager, DeckManager] = {
    deck.moveTopDiscardToTopOfDeck.bimap(fa => copy(deck=fa), fb => copy(deck=fb))
  }

  // Consider Xor
  def moveCardFromHandToBottomOfDeck(cardToUse: Card): DeckManager = {
    val nh = hand.discard(cardToUse)
    val nd = deck.placeAtBottom(cardToUse)

    copy(hand=nh, deck=nd)
  }
}

object DeckManager {

  def createManagerForNewGame(deck: Deck) = DeckManager(Hand.emptyHand, deck)

  def startNewGameWithDefaultDeck: DeckManager = (DeckManager.apply _).tupled(Deck.create.shuffle.deal)

  /**
    * Fetch the deck whatever the outcome was, sometimes we don't care if the action failed (e.g. tests)
    */
  def get(dx: Xor[DeckManager, DeckManager]) = dx.fold(a => a, b => b)
}
