package chousen.html

import chousen.api.data.GameStateGenerator
import org.scalatest.WordSpec
import play.twirl.api.Html


class GameSpec extends WordSpec {

  "Game.html" should {

    val gameState = GameStateGenerator.staticGameState

    val gamePage: Html = chousen.ui.html.game(gameState)

    "State this is a dev client" in {
      assert(gamePage.body contains "Chousen Dev Client")
    }

    "Include the player name" in {
      assert(gamePage.body contains GameStateGenerator.playerName)
    }

  }

}
