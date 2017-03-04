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


    "provided two a player and enemy with equal speeds and different positions" should {

      // Note that the enemy is closer to their next turn
      val player = Player("Player", speed10Char, position = 0)
      val enemy = Enemy("Quick Enemy", UUID.randomUUID(), speed10Char, position = 50)
      val emptyMessages = Seq.empty[GameMessage]

      val (updPlayer, updEnemies, updMessages) =
        GameOps.update(player, Set(enemy), emptyMessages)

      "Set the highest positioned player to be the actor with a higher position" in {
        assert(updEnemies.maxBy(_.position).position > updPlayer.position)
      }

      "Set the lower position character's position to 50" in {
        assert(updPlayer.position == 50)
      }

      "Set the higher position character's position to 100" in {
        assert(updEnemies.exists(_.position == 100))
      }

      "Include a game message stating it's the higher positioned users turn" in {
        assert(updMessages.size == 1)
        assert(updMessages.head.text == "Quick Enemy's turn!")
      }

      "updating again" must {

        val (latestPlayer, latestEnemies, latestMessages) =
          GameOps.update(updPlayer, updEnemies, updMessages)

        "retain the active actor" in {
          assert(updEnemies.maxBy(_.position).position > updPlayer.position)
          assert(latestEnemies.maxBy(_.position).position > latestPlayer.position)
        }

        "Include an additional message with the same text" in {
          assert(latestMessages.size != updEnemies.size)
          assert(latestMessages.size == 2)
          assert(latestMessages.last.text == "Quick Enemy's turn!")

          // Same message
          assert(latestMessages.toSet.size == 1)
        }

      }

      "updating after the enemy has been reset" must {

        val es = updEnemies.map(e => e.copy(position = e.position-100))
        val (latestPlayer, latestEnemies, latestMessages) =
          GameOps.update(updPlayer, es ,
            updMessages)

        "change the active actor" in {
          assert(updEnemies.maxBy(_.position).position > updPlayer.position)
          assert(latestEnemies.maxBy(_.position).position < latestPlayer.position)
        }

        "Include a game message stating it's now the Player's turn" in {
          assert(latestMessages.size == 2)
          assert(latestMessages.exists(_.text == "Player's turn!"))
          assert(latestMessages.last.text == "Player's turn!")
        }

      }

    }

  }

}
