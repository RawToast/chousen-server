package chousen.game.actions

import java.util.UUID

import chousen.api.data._
import chousen.game.core.RandomGameStateCreator
import chousen.game.dungeon.SimpleDungeonBuilder
import org.scalatest.WordSpec
import chousen.Optics._

class CardActionHandlerSpec extends WordSpec {

  "Card Action Handler" when {

    "Given a card action" should {
      val gameState = GameStateGenerator.gameStateWithFastPlayer

      val dungeonBuilder = new SimpleDungeonBuilder()
      val stateCreator = new RandomGameStateCreator(dungeonBuilder)
      val startedGame: GameState = stateCreator.start(gameState)

      val result = CardActionHandler.handle(Rummage, None)(startedGame)

      "State the action was used" in {
        assert(result.messages.size > startedGame.messages.size)
      }

    }

    "Given Rummage" should {
      val gameState = GameStateGenerator.gameStateWithFastPlayer

      val dungeonBuilder = new SimpleDungeonBuilder()
      val stateCreator = new RandomGameStateCreator(dungeonBuilder)
      val startedGame: GameState = stateCreator.start(gameState)

      val result = CardActionHandler.handle(Rummage, None)(startedGame)

      "Draw two cards" in {
        assert(result.cards.hand.size > (1 + startedGame.cards.hand.size))
      }
    }

    "Given Miracle" should {
      val gameState = GameStateGenerator.gameStateWithFastPlayer

      val dungeonBuilder = new SimpleDungeonBuilder()
      val stateCreator = new RandomGameStateCreator(dungeonBuilder)
      val game: GameState = stateCreator.start(gameState)

      val startedGame: GameState = HandLens.set(Seq.empty[Card])(game)

      val result = CardActionHandler.handle(Miracle, None)(startedGame)


      "Draw cards upto the maximum hand size + 1" in {
        assert(result.cards.hand.size > startedGame.cards.hand.size)
        // Afterwards miracle would be discarded
        assert(result.cards.hand.size == 8)
      }
    }

    "Given Replace" should {
      val gameState = GameStateGenerator.gameStateWithFastPlayer

      val dungeonBuilder = new SimpleDungeonBuilder()
      val stateCreator = new RandomGameStateCreator(dungeonBuilder)
      val game: GameState = stateCreator.start(gameState)

      val startedGame: GameState = HandLens.set(Seq.empty[Card])(game)

      val result = CardActionHandler.handle(Replace, None)(startedGame)

      "Give at least 3 cards" in {
        assert(result.cards.hand.size > startedGame.cards.hand.size)
        // Afterwards miracle would be discarded
        assert(result.cards.hand.size >= 3)
      }

      "Replace the current hand" in {
        val result = CardActionHandler.handle(Replace, None)(game)

        assert(result.cards.hand.size == game.cards.hand.size)
        // Afterwards miracle would be discarded
      }
    }

    "Given Restore" should {
      val gameState = GameStateGenerator.gameStateWithFastPlayer

      val dungeonBuilder = new SimpleDungeonBuilder()
      val stateCreator = new RandomGameStateCreator(dungeonBuilder)
      val game: GameState = stateCreator.start(gameState)

      val startedGame: GameState = DiscardLens.set(Seq(Card(UUID.randomUUID(), "Test", "Test", CrushingBlow)))(game)

      val result = CardActionHandler.handle(Restore, None)(startedGame)

      "Place the top discarded card into the player's hand" in {
        assert(!startedGame.cards.hand.exists(_.name == "Test"))
        assert(startedGame.cards.discard.exists(_.name == "Test"))

        assert(result.cards.hand.exists(_.name == "Test"))
        assert(!result.cards.discard.exists(_.name == "Test"))
      }
    }
  }
}
