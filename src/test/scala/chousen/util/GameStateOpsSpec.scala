package chousen.util

import chousen.Optics
import chousen.api.data.{Card, GameStateGenerator}
import chousen.game.cards.CardCatalogue
import org.scalatest.WordSpec

class GameStateOpsSpec extends WordSpec {

  "GameStateOps" when {

    val gameStateOps = new GameStateOps {}
    val gameState = GameStateGenerator.staticGameState

    "Turning a GameState into a GameResponse" should {

      val result = gameStateOps.toGameResponse(gameState, Seq.empty)

      "Retain the game ID" in {
        assert(result.uuid == gameState.uuid)
      }

      "Retain all player information" in {
        assert(result.player == gameState.player)
      }

      "Retain all messages" in {
        assert(result.messages == gameState.messages.map(_.text))
      }

      "Retain the Player's hand size" in {
        assert(result.cards.hand.size == gameState.cards.hand.size)
      }

      //TODO: Add card checks

      "Return the current encounter" in {
        assert(result.currentEncounter == gameState.dungeon.currentEncounter)
      }

      "Handle complex cards" in {

        val cards: Seq[Card] = Seq(CardCatalogue.pickACard, CardCatalogue.findersKeepers,
          CardCatalogue.anotherTime, CardCatalogue.essenceBoost, CardCatalogue.reduceRequirements,
          CardCatalogue.greatSword, CardCatalogue.fireball)

        val initialState = chousen.Optics.HandLens.set(cards)(gameState)

        val result = gameStateOps.toGameResponse(initialState, Seq.empty)


        assert(result.cards.hand.size == cards.size)

      }
    }

    "An Essence has been played" should {

      val essencePlayedState = Optics.CardsLens.modify(c => c.copy(playedEssence = true, hand = c.hand :+ CardCatalogue.essenceOfVitality))(gameState)

      val result = gameStateOps.toGameResponse(essencePlayedState, Seq.empty)

      "The player still has Essence cards" in {
        assert(result.cards.hand.exists(_.name.contains("Essence of")))
      }

      "Disable the Player's Essence cards" in {
        assert(result.cards.hand.filter(_.name.contains("Essence of")).forall(_.playable == false))
      }

      "Leave non Essences playable" in {
        assert(result.cards.hand.filterNot(_.name.contains("Essence of")).exists(_.playable == true))
      }
    }
  }
}

// case class GameState(uuid: UUID, player: Player, cards: Cards, dungeon: Dungeon, messages: Seq[GameMessage])
//case class Cards(hand: Seq[Card], deck: Seq[Card], discard: Seq[Card], passive: Seq[Card], equippedCards: EquippedCards, playedEssence:Boolean=false)
//case class Card(id: UUID, name: String, description: String, action: Action, charges: Option[Int]=None, maxCharges:Option[Int]=None, requirements: Requirements=Requirements())
