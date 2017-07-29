package chousen.game.actions

  import chousen.api.data._
import chousen.Optics._
  import chousen.game.core.RandomGameStateCreator
  import chousen.game.dungeon.SimpleDungeonBuilder
  import org.scalatest.WordSpec

  class CampFireActionHandlerSpec extends WordSpec {

    "CampFireActionHandler" when {
      val dungeonBuilder = new SimpleDungeonBuilder()
      val stateCreator = new RandomGameStateCreator(dungeonBuilder)

      "Given a CampFireAction when there are enemies" should {

        val initialState: GameState = GameStateGenerator.gameStateWithFastPlayer
        val startedGame: GameState = stateCreator.start(initialState)

        lazy val result = CampFireActionHandler.handle(Explore).apply(startedGame)

        "Have no affect on the player" in {
          assert(result.player == startedGame.player)
        }

        "Have no affect on the enemies" in {
          assert(result.dungeon == startedGame.dungeon)
        }

        "Have no affect on the deck" in {
          assert(result.cards == startedGame.cards)
        }

        "Have no affect on messages" in {
          assert(result.messages == startedGame.messages)
        }
      }

      "Given a CampFireAction when there is only a CampFire" should {

        val initialState: GameState = GameStateGenerator.gameStateWithFastPlayer
        val game: GameState = stateCreator.start(initialState)

        val startedGame =
          DungeonLens
            .set(game.dungeon.copy(currentEncounter = Battle(Set(dungeonBuilder.campFire))))
            .apply(game)

        lazy val result = CampFireActionHandler.handle(RestAndExplore).apply(startedGame)

        "Have an affect on messages" in {
          assert(startedGame.cards.passive.size == 3)
          assert(result.messages != startedGame.messages)
        }

        "The player moves to the next encounter" in {
          assert(result.dungeon.currentEncounter != startedGame.dungeon.currentEncounter)
        }
      }



      "Rest is used" should {
        val initialState: GameState = GameStateGenerator.gameStateWithFastPlayer
        val game: GameState = stateCreator.start(initialState)

        val startedGame =
          DungeonLens
            .set(game.dungeon.copy(currentEncounter = Battle(Set(dungeonBuilder.campFire))))
            .compose(PlayerLens.composeLens(PlayerHealthLens).modify(hp => hp / 2))(game)

        lazy val result = CampFireActionHandler.handle(Rest).apply(startedGame)

        "Have an affect on messages" in {
          assert(result.messages != startedGame.messages)
        }

        "The card remains available" in {
          assert(result.cards.passive.size == startedGame.cards.passive.size)
          assert(result.cards.passive.exists(_.action == Rest))
        }

        "Heal the player" in {
          assert(result.player.stats.currentHp == result.player.stats.maxHp)
          assert(result.player.stats.currentHp != startedGame.player.stats.currentHp)
        }
      }

      "Rest and Explore is used" should {
        val initialState: GameState = GameStateGenerator.gameStateWithFastPlayer
        val game: GameState = stateCreator.start(initialState)

        val startedGame =
          DungeonLens
            .set(game.dungeon.copy(currentEncounter = Battle(Set(dungeonBuilder.campFire))))
            .compose(PlayerLens.composeLens(PlayerHealthLens).modify(hp => hp / 2))(game)

        lazy val result = CampFireActionHandler.handle(RestAndExplore).apply(startedGame)

        "Have an affect on messages" in {
          assert(result.messages != startedGame.messages)
        }

        "The card remains available" in {
          assert(result.cards.passive.size == startedGame.cards.passive.size)
          assert(result.cards.passive.exists(_.action == RestAndExplore))
        }

        "Heal the player" in {
          assert(result.player.stats.currentHp > startedGame.player.stats.currentHp)
        }

        "Draw a card" in {
          assert(result.cards.hand.size > startedGame.cards.hand.size)
        }
      }

      "Explore is used" should {
        val initialState: GameState = GameStateGenerator.gameStateWithFastPlayer
        val game: GameState = stateCreator.start(initialState)

        val startedGame =
          DungeonLens
            .set(game.dungeon.copy(currentEncounter = Battle(Set(dungeonBuilder.campFire))))
            .compose(PlayerLens.composeLens(PlayerHealthLens).modify(hp => hp / 2))(game)

        lazy val result = CampFireActionHandler.handle(Explore).apply(startedGame)

        "Have an affect on messages" in {
          assert(result.messages != startedGame.messages)
        }

        "The card remains available" in {
          assert(result.cards.passive.size == startedGame.cards.passive.size)
          assert(result.cards.passive.exists(_.action == Explore))
        }

        "Draw two cards" in {
          assert(result.cards.hand.size > (1 + startedGame.cards.hand.size))
        }
      }


    }

  }
