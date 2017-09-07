package chousen.game.core.turn

import chousen.api.data.{CharStats, Equipment, Experience, Player}
import org.scalatest.WordSpec

class PostTurnOpsSpec extends WordSpec {

  "PostTurnOps" when {

    "The Player has 0 experience" should {

      lazy val inexperiencedPlayer = Player("Test", "Test", CharStats(5, 5), Experience(), Equipment(), 0, 0)
      lazy val (newPlayer, messages) = PostTurnOps.levelUp(inexperiencedPlayer)

      "Make no changes to the player" in {
        assert(newPlayer == inexperiencedPlayer)
      }

      "Create no game messages" in {
        assert(messages.isEmpty)
      }
    }


    "The Player has enough experience to level up" should {

      lazy val experiencedPlayer = Player("Test", "Test", CharStats(5, 5), Experience(current = 3), Equipment(), 0, 0)
      lazy val (newPlayer, messages) = PostTurnOps.levelUp(experiencedPlayer)

      "Return an amended player" in {
        assert(newPlayer != experiencedPlayer)
      }

      "Create a level-up game message" in {
        assert(messages.nonEmpty)
      }

      "Level up the player" in {
        assert(newPlayer.experience.level > experiencedPlayer.experience.level)
      }
    }

    "The Player has enough experience to level up multiple times" should {

      val experiencedPlayer = Player("Test", "Test", CharStats(5, 5), Experience(current = 10), Equipment(), 0, 0)
      val (newPlayer, messages) = PostTurnOps.levelUp(experiencedPlayer)

      "Return an amended player" in {
        assert(newPlayer != experiencedPlayer)
      }

      "Create a level-up game message" in {
        assert(messages.nonEmpty)
      }

      "Level up the player more than once" in {
        assert(newPlayer.experience.level > experiencedPlayer.experience.level + 1)
      }
    }

  }

}
