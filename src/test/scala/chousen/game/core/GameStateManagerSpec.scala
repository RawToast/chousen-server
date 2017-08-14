package chousen.game.core

import java.util.UUID

import chousen.api.data.{GameStateGenerator, _}
import chousen.game.actions.DamageCalculator
import chousen.game.cards.CardCatalogue
import chousen.game.dungeon.SimpleDungeonBuilder
import chousen.game.status.StatusCalculator
import org.scalatest.WordSpec

class GameStateManagerSpec extends WordSpec {

  "GameStateManager" when {

    val sc = new StatusCalculator()
    val damageCalculator = new DamageCalculator(sc)
    val gameStateManager = new GameStateManager(damageCalculator)
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

          assert(latestMessages.head == GameMessage("Test Player jumps in the air and lands a crushing blow to Slime!"))
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

    "Accepting equipment card" when {

      "The player is already equipped" should {
        import chousen.Optics._

        val broardsword = CardCatalogue.broadsword

        val swordOfIntellect = CardCatalogue.swordOfIntellect
        val swordId = swordOfIntellect.id
        val request = EquipmentActionRequest(swordId, SwordOfIntellect)


        val initialState = GameStateOptics.HandLens.modify(_ :+ swordOfIntellect)
          .andThen(PlayerLens.composeLens(PlayerWeaponLens).set(Option(Weapon(broardsword.id, "Broadsword", 10))))
            .andThen(EquipmentLens.set(EquippedCards(Option(broardsword))))(gameState)

        lazy val result = gameStateManager.useCard(swordOfIntellect, request, initialState)

        "Equip the new card" in {
          assert(result.cards.equippedCards.weapon != initialState.cards.equippedCards.weapon)
        }

        "Place the old item in the Player's hand" in {
          assert(!initialState.cards.hand.contains(broardsword))
          assert(result.cards.hand.contains(broardsword))
        }
      }
    }

  }

  def getFirstEnemyHp(result: GameState) =
    result.dungeon.currentEncounter.enemies
      .find(_.id == GameStateGenerator.firstEnemy.id)
      .map(_.stats.currentHp).getOrElse(404)
}
