package chousen.util

import chousen.Optics
import chousen.api.data.GameStateGenerator
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
    }

    "An Essence has been played" should {

      val essencePlayedState = Optics.CardsLens.modify(c => c.copy(playedEssence = true))(gameState)

      val result = gameStateOps.toGameResponse(essencePlayedState, Seq.empty)

      "Disable the Player's Essence cards" in {
        assert(result.cards.hand.filter(_.name.contains("Essence")).forall(_.playable == false))
      }

      "Leave non Essences playable" in {
        assert(result.cards.hand.filterNot(_.name.contains("Essence")).exists(_.playable == true))
      }
    }
  }
}

// case class GameState(uuid: UUID, player: Player, cards: Cards, dungeon: Dungeon, messages: Seq[GameMessage])
//case class Cards(hand: Seq[Card], deck: Seq[Card], discard: Seq[Card], passive: Seq[Card], equippedCards: EquippedCards, playedEssence:Boolean=false)
//case class Card(id: UUID, name: String, description: String, action: Action, charges: Option[Int]=None, maxCharges:Option[Int]=None, requirements: Requirements=Requirements())
