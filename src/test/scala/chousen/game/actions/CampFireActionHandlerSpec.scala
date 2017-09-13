package chousen.game.actions

  import chousen.api.data._
  import chousen.Optics._
  import chousen.game.cards.CardCatalogue
  import chousen.game.core.RandomGameStateCreator
  import chousen.game.dungeon.SimpleDungeonBuilder
  import org.scalatest.WordSpec

  class CampFireActionHandlerSpec extends WordSpec {

    "CampFireActionHandler" when {
      val dungeonBuilder = new SimpleDungeonBuilder()
      val stateCreator = new RandomGameStateCreator(dungeonBuilder)

      val initialState: GameState = GameStateGenerator.gameStateWithFastPlayer
      val game: GameState = stateCreator.start(initialState)

      "Given a CampFireAction when there are enemies" should {

        lazy val result = CampFireActionHandler.handle(Explore, None).apply(game)

        "Have no affect on the player" in {
          assert(result.player == game.player)
        }

        "Have no affect on the enemies" in {
          assert(result.dungeon == game.dungeon)
        }

        "Have no affect on the deck" in {
          assert(result.cards == game.cards)
        }

        "Have no affect on messages" in {
          assert(result.messages == game.messages)
        }
      }

      "Given a CampFireAction when there is only a CampFire" should {

        val startedGame =
          DungeonLens
            .set(game.dungeon.copy(currentEncounter = Battle(Set(dungeonBuilder.campFire))))
            .apply(game)

        lazy val result = CampFireActionHandler.handle(RestAndExplore, None).apply(startedGame)

        "Does not consume the passive action card" in {
          assert(startedGame.cards.passive.size == 6)
          assert(result.cards.passive.size == 6)
        }

        "Have an affect on messages" in {
          assert(result.messages != startedGame.messages)
        }

        "The player moves to the next encounter" in {
          assert(result.dungeon.currentEncounter != startedGame.dungeon.currentEncounter)
        }
      }



      "Rest is used" should {

        val startedGame =
          DungeonLens
            .set(game.dungeon.copy(currentEncounter = Battle(Set(dungeonBuilder.campFire))))
            .compose(PlayerLens.composeLens(PlayerHealthLens).modify(hp => hp / 2))(game)

        lazy val result = CampFireActionHandler.handle(Rest, None).apply(startedGame)

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

        lazy val result = CampFireActionHandler.handle(RestAndExplore, None).apply(startedGame)

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

        val startedGame =
          DungeonLens
            .set(game.dungeon.copy(currentEncounter = Battle(Set(dungeonBuilder.campFire))))
            .compose(PlayerLens.composeLens(PlayerHealthLens).modify(hp => hp / 2))(game)

        lazy val result = CampFireActionHandler.handle(Explore, None).apply(startedGame)

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

      "Drop is used" should {
        val initialState: GameState = GameStateGenerator.gameStateWithFastPlayer
        val game: GameState = stateCreator.start(initialState)

        val cardToDiscard = CardCatalogue.cape

        val startedGame =
          DungeonLens
            .set(game.dungeon.copy(currentEncounter = Battle(Set(dungeonBuilder.campFire))))
            .andThen(PlayerLens.composeLens(PlayerHealthLens).modify(hp => hp / 2))
            .andThen(HandLens.modify(_ :+ cardToDiscard))(game)


        lazy val result = CampFireActionHandler.handle(Drop, Some(cardToDiscard.id)).apply(startedGame)

        "Have an affect on messages" in {
          assert(result.messages != startedGame.messages)
        }

        "The card remains available" in {
          assert(result.cards.passive.size == startedGame.cards.passive.size)
          assert(result.cards.passive.exists(_.action == Drop))
        }

        "Hand size is reduced" in {
          assert(startedGame.cards.hand.size > result.cards.hand.size)
        }

        "Card placed in discard pile" in {
          assert(result.cards.discard.exists(_.id == cardToDiscard.id))
        }

        "Add a  if the card is not in the users hand" in {
          val r = CampFireActionHandler.handle(Drop, None).apply(startedGame)
          assert(r == startedGame)
        }
      }

      "Destroy is used" should {

        val startedGame =
          DungeonLens
            .set(game.dungeon.copy(currentEncounter = Battle(Set(dungeonBuilder.campFire))))
            .compose(PlayerLens.composeLens(PlayerHealthLens).modify(hp => hp / 2))(game)

        lazy val cardToDiscard = startedGame.cards.hand.head

        lazy val result = CampFireActionHandler.handle(Destroy, Some(cardToDiscard.id)).apply(startedGame)

        "Have an affect on messages" in {
          assert(result.messages != startedGame.messages)
        }

        "The card remains available" in {
          assert(result.cards.passive.size == startedGame.cards.passive.size)
          assert(result.cards.passive.exists(_.action == Destroy))
        }

        "Hand size is reduced" in {
          assert(startedGame.cards.hand.size > result.cards.hand.size)
        }

        "Card is removed from the hand" in {
          assert(!result.cards.hand.exists(_.id == cardToDiscard.id))
        }

        "Card is not placed in discard pile" in {
          assert(!result.cards.discard.exists(_.id == cardToDiscard.id))
        }

        "Card is not placed in the deck" in {
          assert(!result.cards.deck.exists(_.id == cardToDiscard.id))
        }


        "Have no effect, if the card is not in the users hand" in {
          val r = CampFireActionHandler.handle(Destroy, None).apply(startedGame)
          assert(r.messages == startedGame.messages)
          assert(r.player == startedGame.player)
          assert(r.uuid == startedGame.uuid)
          assert(r.cards == startedGame.cards)
          assert(r.dungeon.currentEncounter == startedGame.dungeon.currentEncounter)
        }

      }

      "Learn Skill is used" should {

        val initialState: GameState = PlayerLens.composeLens(PlayerIntellectLens)
          .set(10)(GameStateGenerator.gameStateWithFastPlayer)

        val game: GameState = stateCreator.start(initialState)

        val cardToDiscard = CardCatalogue.bankruptcy

        val startedGame =
          DungeonLens
            .set(game.dungeon.copy(currentEncounter = Battle(Set(dungeonBuilder.campFire))))
            .andThen(PlayerLens.composeLens(PlayerHealthLens).modify(hp => hp / 2))
            .andThen(HandLens.modify(_ :+ cardToDiscard))(game)


        lazy val result = CampFireActionHandler.handle(LearnSkill, Some(cardToDiscard.id)).apply(startedGame)

        "Have an affect on messages" in {
          assert(result.messages != startedGame.messages)
        }

        "The card remains available" in {
          assert(result.cards.passive.size == startedGame.cards.passive.size)
          assert(result.cards.passive.exists(_.action == LearnSkill))
        }

        "Hand size is reduced" in {
          assert(startedGame.cards.hand.size > result.cards.hand.size)
        }

        "Card placed in equipped cards" in {
          assert(result.cards.equippedCards.skills.exists(_.id == cardToDiscard.id))
        }

        "add a message if the player does not have enough Int" in {

          val staticGame = stateCreator.start(GameStateGenerator.gameStateWithFastPlayer)
          val game = DungeonLens
            .set(staticGame.dungeon.copy(currentEncounter = Battle(Set(dungeonBuilder.campFire))))(staticGame)
          val r = CampFireActionHandler.handle(LearnSkill, None).apply(game)
          assert(r != startedGame)
        }
      }

    }

  }
