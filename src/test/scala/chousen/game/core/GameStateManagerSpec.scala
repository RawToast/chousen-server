package chousen.game.core

import java.util.UUID

import chousen.api.data.{GameStateGenerator, _}
import chousen.game.actions.DamageCalculator
import chousen.game.cards.CardCatalogue
import chousen.game.dungeon.SimpleDungeonBuilder
import chousen.game.status.{PostTurnStatusCalculator, StatusCalculator}
import org.scalatest.WordSpec

class GameStateManagerSpec extends WordSpec {

  "GameStateManager" when {

    val sc = new StatusCalculator()
    val damageCalculator = new DamageCalculator(sc)
    val postTurnStatusCalc = new PostTurnStatusCalculator
    val gameStateManager = new GameStateManager(damageCalculator, postTurnStatusCalc)
    val dungeonBuilder = new SimpleDungeonBuilder()
    val gameStateCreator = new RandomGameStateCreator(dungeonBuilder)
    val gameState = GameStateGenerator.gameStateWithFastPlayer
    val startedGame: GameState = gameStateCreator.start(gameState)
    val encOps = new EncounterOp(new StatusCalculator)

    "Accepting a command" which {
      "Is a basic attack" should {
        val target = GameStateGenerator.firstEnemy
        val result = gameStateManager.takeCommand(AttackRequest(target.id), startedGame)

        "Lower the targeted enemies health" in {
          assert(startedGame.dungeon.currentEncounter.enemies.exists(_.id == target.id))
          assert(startedGame.dungeon.currentEncounter.enemies.size == 2)
          assert(result.dungeon.currentEncounter.enemies.size == 2)
          assert(getFirstEnemyHp(result) < getFirstEnemyHp(startedGame))
        }

        "the player is set back to active" in {
          assert(result.player.position > 100)
          val active = encOps.getActive((result.player,
            result.dungeon.currentEncounter.enemies, result.messages))
          assert(active.isLeft)

          import chousen.Implicits._
          active.swap.foreach(_ ~= startedGame.player)
        }

        lazy val numberOfNewMessages = result.messages.size - startedGame.messages.size
        lazy val latestMessages = result.messages.takeRight(numberOfNewMessages)

        "game messages are created for the player's attack" in {
          assert(result.messages.size > startedGame.messages.size)

          assert(latestMessages.head == GameMessage("Test Player attacks Slime."))
          assert(latestMessages(1).text.contains("Test Player's attack deals"))
        }

        "the enemy takes their turn" in {
          assert(result.player.stats.currentHp < startedGame.player.stats.currentHp)

          assert(latestMessages.exists(_.text.contains("Slime grazes Test Player")))
          assert(latestMessages.exists(_.text.contains(" damage.")))
        }
      }

      "Is a block command" should {
        val result = gameStateManager.takeCommand(BlockRequest(), startedGame)

        "the player is set back to active" in {
          assert(result.player.position > 100)
          val active = encOps.getActive((result.player,
            result.dungeon.currentEncounter.enemies, result.messages))
          assert(active.isLeft)

          import chousen.Implicits._
          active.swap.foreach(_ ~= startedGame.player)
        }

        lazy val numberOfNewMessages = result.messages.size - startedGame.messages.size
        lazy val latestMessages = result.messages.takeRight(numberOfNewMessages)

        "game messages are created for the action" in {
          assert(result.messages.size > startedGame.messages.size)
        }

        "the enemy takes their turn" in {
          assert(result.player.stats.currentHp < startedGame.player.stats.currentHp)

          assert(latestMessages.exists(_.text.contains("Slime grazes Test Player")))
          assert(latestMessages.exists(_.text.contains(" damage.")))
        }
      }

      "Is a basic single target command" should {
        val target = GameStateGenerator.firstEnemy
        val singleTargetCommand =  SingleTargetActionRequest(target.id, CrushingBlow)
        val result = gameStateManager.takeCommand(singleTargetCommand, startedGame)

        "Lower the targeted enemies health" in {
          assert(startedGame.dungeon.currentEncounter.enemies.exists(_.id == target.id))
          assert(startedGame.dungeon.currentEncounter.enemies.size == 2)
          assert(result.dungeon.currentEncounter.enemies.size == 2)
          assert(getFirstEnemyHp(result) < getFirstEnemyHp(startedGame))
        }

        "the player is set back to active" in {
          assert(result.player.position > 100)
          val active = encOps.getActive((result.player,
            result.dungeon.currentEncounter.enemies, result.messages))
          assert(active.isLeft)

          import chousen.Implicits._
          active.swap.foreach(_ ~= startedGame.player)
        }

        lazy val numberOfNewMessages = result.messages.size - startedGame.messages.size
        lazy val latestMessages = result.messages.takeRight(numberOfNewMessages)

        "game messages are created for the player's attack" in {
          assert(result.messages.size > startedGame.messages.size)

          assert(latestMessages.head == GameMessage("Test Player lands a crushing blow on Slime!"))
          assert(latestMessages(1).text.contains("Slime takes"))
        }

        "the enemy takes their turn" in {
          assert(result.player.stats.currentHp < startedGame.player.stats.currentHp)

          assert(latestMessages.exists(_.text.contains("Slime grazes Test Player")))
          assert(latestMessages.exists(_.text.contains(" damage.")))
        }
      }
    }

    "Accepting a card" which {

      "The user does not have" should {

        "Return the game state with no changes" in {
          val anotherCard = GameStateGenerator.crushingBlowCard.copy(id = UUID.fromString("221c878f-5a6f-4276-a52e-862cfa90e114"))

          val request = SingleTargetActionRequest(GameStateGenerator.firstEnemy.id, CrushingBlow)

          val result = gameStateManager.useCard(anotherCard, request, gameState)

          assert(result == gameState)
        }
      }

      "The user does not meet the requirements for" should {

        lazy val anotherCard = GameStateGenerator.crushingBlowCard
          .copy(id = UUID.fromString("221c878f-5a6f-4276-a52e-862cfa90e114"), requirements = Requirements(str=Some(99)))

        lazy val request = SingleTargetActionRequest(GameStateGenerator.firstEnemy.id, CrushingBlow)

        lazy val result = gameStateManager.useCard(anotherCard, request, gameState)

        "Return the game state with changes" in {
          assert(gameState != result)
        }

        "Add a game message stating the player cannot use the card" in {
          assert(result.messages.size > gameState.messages.size)
          assert(result.messages.exists(_.text.contains("Cannot use")))
        }

        "Not affect the player" in {
          assert(result.player == gameState.player)
        }

        "Not affect the current encounter" in {
          assert(result.dungeon.currentEncounter == gameState.dungeon.currentEncounter)
        }
      }

      "Is different to the specified action" should {

        "Return the game state with no changes" in {
          lazy val incorrectCard = GameStateGenerator.crushingBlowCard
          val request = SingleTargetActionRequest(GameStateGenerator.firstEnemy.id, StunningStrike)

          val result = gameStateManager.useCard(incorrectCard, request, gameState)

          assert(result == gameState)
        }
      }

      "Is a valid single target request" should {

        lazy val card = GameStateGenerator.crushingBlowCard

        val initialState = GameStateOptics.HandLens.modify(_ :+ card)(gameState)

        val request = SingleTargetActionRequest(GameStateGenerator.firstEnemy.id, CrushingBlow)

        lazy val result = gameStateManager.useCard(card, request, initialState)

        "Change the game state" in {
          assert(result != initialState)
        }

        "Remove the card from the player's hand" in {
          //assert(initialState.cards.hand.size > result.cards.hand.size)
          assert(!result.cards.hand.contains(card))
        }
      }

      "Is a valid single target request for a card with charges" should {

        lazy val card = GameStateGenerator.crushingBlowCard.copy(charges = Option(2))

        val initialState = GameStateOptics.HandLens.modify(_ :+ card)(gameState)

        val request = SingleTargetActionRequest(GameStateGenerator.firstEnemy.id, CrushingBlow)

        lazy val result = gameStateManager.useCard(card, request, initialState)

        "Change the game state" in {
          assert(result != initialState)
        }

        "Does not remove the card from the player's hand" in {
          assert(result.cards.hand.exists(_.id == card.id))
        }

        "Reduces the number of charges" in {
          assert(result.cards.hand.find(_.id == card.id)
            .flatMap(_.charges) == Option(1))
        }

        "Discards the card if it has a single charge" in {
          lazy val singleChargeCard = GameStateGenerator.crushingBlowCard.copy(charges = Option(1))
          val testState = GameStateOptics.HandLens.modify(_ :+ singleChargeCard)(gameState)
          val r2 = SingleTargetActionRequest(GameStateGenerator.firstEnemy.id, CrushingBlow)

          lazy val result2 = gameStateManager.useCard(singleChargeCard.copy(charges = Option(1)), r2, testState)


          assert(!result2.cards.hand.exists(_.id == singleChargeCard.id))
        }
      }

      "Is a valid multi target request for a card with charges" should {

        lazy val card = GameStateGenerator.fireballCard.copy(charges = Option(2))

        val initialState = GameStateOptics.HandLens.modify(_ :+ card)(gameState)

        val request = MultiTargetActionRequest(
          Set(GameStateGenerator.firstEnemy.id, GameStateGenerator.secondEnemy.id), Fireball)

        lazy val result = gameStateManager.useCard(card, request, initialState)

        "Change the game state" in {
          assert(result != initialState)
        }

        "Does not remove the card from the player's hand" in {
          assert(result.cards.hand.exists(_.id == card.id))
        }

        "Reduces the number of charges" in {
          assert(result.cards.hand.find(_.id == card.id)
            .flatMap(_.charges) == Option(1))
        }
      }

      "Is a valid self targeted request" should {

        lazy val card = CardCatalogue.rarePepe

        val initialState = GameStateOptics.HandLens.modify(_ :+ card)(gameState)

        val request = SelfInflictingActionRequest(RarePepe)

        lazy val result = gameStateManager.useCard(card, request, initialState)

        "Changes the game state" in {
          assert(result != initialState)
        }

        "Remove's the card from the player's hand" in {
          assert(!result.cards.hand.exists(_.id == card.id))
        }
      }

      "Is a valid discard action" should {

        lazy val card = CardCatalogue.essenceBoost
        lazy val toDiscard = GameStateGenerator.fireballCard

        val initialState = GameStateOptics.HandLens.modify(_ :+ card :+ toDiscard)(gameState)

        val request = CardActionRequest(EssenceBoost, Option(toDiscard.id))

        lazy val result = gameStateManager.useCard(card, request, initialState)

        "Change the game state" in {
          assert(result != initialState)
        }

        "Remove both cards from the player's hand" in {
          //assert(initialState.cards.hand.size > result.cards.hand.size)
          assert(!result.cards.hand.contains(card))
          assert(!result.cards.hand.contains(toDiscard))
        }
      }

    }

    "Accepting an Essence card" when {


      "One has already been played" should {
        lazy val card = CardCatalogue.essenceOfIntelligence

        val state1 = GameStateOptics.HandLens.modify(_ :+ card)(gameState)

        val initialState = state1.copy(cards = state1.cards.copy(playedEssence = true))

        val request = SelfInflictingActionRequest(EssenceOfIntelligence)

        lazy val result = gameStateManager.useCard(card, request, initialState)

        "Change the game state" in {
          assert(result != initialState)
        }

        "Add a warning message to the game" in {
          //assert(initialState.cards.hand.size > result.cards.hand.size)
          assert(result.messages.size > initialState.messages.size)
        }

        "Retain the card in the Player's hand" in {
          //assert(initialState.cards.hand.size > result.cards.hand.size)
          assert(result.cards.hand.contains(card))
        }

      }
    }

    "Accepting an action card" when {

      "The player is berserk" should {

        import chousen.Optics.{PlayerStatusLens, PlayerLens}

        val initialState = (PlayerLens ^|-> PlayerStatusLens).set(Seq(Status(Rage, "test", 4, Some(5))))(gameState)
        val anotherCard = GameStateGenerator.crushingBlowCard.copy(id = UUID.fromString("221c878f-5a6f-4276-a52e-862cfa90e114"))

        val request = SingleTargetActionRequest(GameStateGenerator.firstEnemy.id, CrushingBlow)

        lazy val result = gameStateManager.useCard(anotherCard, request, initialState)

        "Change the game state" in {
          assert(result != initialState)
        }

        "Add a new message" in {
          assert(result.messages.size > initialState.messages.size)
        }

        "Not affect the current encounter" in {
          assert(result.dungeon.currentEncounter == initialState.dungeon.currentEncounter)
        }
      }
    }
      "The player is already equipped" should {
        import chousen.Optics._

        val shortsword = CardCatalogue.shortSword

        val club = CardCatalogue.club
        val swordId = club.id
        val request = EquipmentActionRequest(swordId, Club)


        val initialState = GameStateOptics.HandLens.modify(_ :+ club)
          .andThen(PlayerLens.composeLens(PlayerWeaponLens).set(Option(Weapon(shortsword.id, "Broadsword", 10))))
            .andThen(EquipmentLens.set(EquippedCards(Option(shortsword))))(gameState)

        lazy val result = gameStateManager.useCard(club, request, initialState)

        "Equip the new card" in {
          assert(result.cards.equippedCards.weapon != initialState.cards.equippedCards.weapon)
        }

        "Place the old item in the Player's hand" in {
          assert(!initialState.cards.hand.contains(shortsword))
          assert(result.cards.hand.contains(shortsword))
        }
      }
  }

  def getFirstEnemyHp(result: GameState) =
    result.dungeon.currentEncounter.enemies
      .find(_.id == GameStateGenerator.firstEnemy.id)
      .map(_.stats.currentHp).getOrElse(404)
}
