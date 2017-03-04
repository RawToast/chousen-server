package chousen.core

import java.util.UUID

import api.data.{CharStats, Enemy, GameMessage, Player}
import org.scalatest.WordSpec

class GameOpsSpec extends WordSpec {

  def speed10Char = CharStats(100, 100, speed = 10)

  def speed8Char = CharStats(100, 100, speed = 8)

  "GameOps.update" when {

    "provided with a fast player and a slow enemy" should {
      val player = Player("Player", speed10Char, position = 0)
      val enemy = Enemy("Enemy", UUID.randomUUID(), speed8Char, position = 0)
      val emptyMessages = Seq.empty[GameMessage]

      val (updPlayer, updEnemies, updMessages) = GameOps.update(player, Set(enemy), emptyMessages)

      "Set the fast player to be the active character" in {
        assert(!updEnemies.exists(e => e.position > updPlayer.position))
      }

      "Set the slow enemy's position to 80" in {
        assert(updEnemies.head.position == 80)
      }

      "Set the fast player's position to 100" in {
        assert(updPlayer.position >= 100)
      }

      "Includes a game message stating it's the fast player's turn" in {
        assert(updMessages.size == 1)
        assert(updMessages.head.text == "Player's turn!")
      }

    }

  }

}
