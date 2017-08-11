package chousen.game.actions

import java.util.UUID

import chousen.api.data._
import chousen.game.core.RandomGameStateCreator
import chousen.game.dungeon.SimpleDungeonBuilder
import org.scalatest.WordSpec


class EquipmentActionHandlerSpec extends WordSpec {


  "Equipment Action Handler" when {

   // val sc = new StatusCalculator
    val equipActionHandler = new EquipmentActionHandler()
    lazy val uuid = UUID.randomUUID()

    "Given an equipment action" should {
      val gameState = GameStateGenerator.gameStateWithFastPlayer

      val dungeonBuilder = new SimpleDungeonBuilder()
      val stateCreator = new RandomGameStateCreator(dungeonBuilder)
      val startedGame: GameState = stateCreator.start(gameState)

      val result = equipActionHandler.handle(BroadSword, uuid)(startedGame)

      lazy val numberOfNewMessages = result.messages.size - startedGame.messages.size
      lazy val latestMessages = result.messages.takeRight(numberOfNewMessages)

      "State the action was used" in {
        result.messages.foreach(m => println(m.text))
        assert(result.messages.size > startedGame.messages.size)
        assert(result.messages.contains(GameMessage(s"${GameStateGenerator.playerName} equips a Broad Sword.")))
      }

      "Reduce the player's position" in {
        assert(result.player.position < 100)
      }

      "Equips the item" in {
        assert(startedGame.player.equipment.weapon.isEmpty)
        assert(result.player.equipment.weapon.nonEmpty)
      }

      "the enemy does not take a turn" in {
        assert(result.player.stats.currentHp == startedGame.player.stats.currentHp)

        assert(latestMessages.exists(!_.text.contains("Slime ")))
      }

//      "the equivalent card is placed into the passive deck" in {
//        assert(result.cards.passive.size > startedGame.cards.passive.size)
//      }
    }

    "Given a weapon EquipAction when the player is already equipped" should {
      val gameState = GameStateGenerator.gameStateWithFastPlayer

      val dungeonBuilder = new SimpleDungeonBuilder()
      val stateCreator = new RandomGameStateCreator(dungeonBuilder)

      val startedGame: GameState = stateCreator.start(gameState)

      import chousen.Optics._
      PlayerLens.composeLens(PlayerWeaponLens).set(Option(Weapon(UUID.randomUUID(), "Test", Requirements())))(startedGame)

      val result = equipActionHandler.handle(GiantClub, uuid)(startedGame)

      "State the action was used" in {
        assert(result.messages.size > startedGame.messages.size)
      }

      "Reduce the player's position" in {
        assert(result.player.position < 100)
      }

      "Equips the item" in {
        assert(result.player.equipment.weapon.nonEmpty)
        assert(result.player.equipment.weapon != startedGame.player.equipment.weapon)
      }
    }

  }

}
