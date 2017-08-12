package chousen.game.actions

import chousen.api.data.{CharStats, Experience, Player}
import org.scalatest.WordSpec

class ActionHandlerSpec extends WordSpec {

  "ActionHandler" when {

    val actionHandler = new ActionHandler {}


    "The Player has enough experience to level up multiple times" should {

      lazy val experiencedPlayer = Player("Test", "Test", CharStats(5, 5), Experience(current = 10), 0)
      lazy val (newPlayer, messages) = actionHandler.levelUp(experiencedPlayer)

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