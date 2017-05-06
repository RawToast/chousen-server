package chousen.game.core

import chousen.api.data.{CharStatsOptics, GameStateGenerator, PlayerOptics}
import org.scalatest.WordSpec

class GameOperationsSpec extends WordSpec {

  "GameOperations.PreTurnValidation" when {

    val activeGame = GameStateGenerator.staticGameState
    val deadPlayerLens = GameStateOptics.PlayerLens.composeLens(PlayerOptics.charStats.composeLens(CharStatsOptics.hp)).set(0)
    val removeEnemies = GameStateOptics.EncounterLens.modify(pem => (pem._1, Set.empty, pem._3))

    "Given an active game" must {

      lazy val result = GameOperations.PreTurnValidation(activeGame)

      "Return a Right sided Either" in {
        assert(result.isRight)
      }
    }

    "Given a game with a dead player" must {
      lazy val result = GameOperations.PreTurnValidation(deadPlayerLens(activeGame))

      "Return a Left sided Either" in {
        assert(result.isLeft)
      }
    }

    "Given a game with a dead enemies" must {
      lazy val result = GameOperations.PreTurnValidation(removeEnemies(activeGame))

      "Return a Left sided Either" in {
        assert(result.isLeft)
      }
    }

  }

  "GameOperations.PostTurnValidation" when {

    val activeGame = GameStateGenerator.staticGameState
    val deadPlayerLens = GameStateOptics.PlayerLens.composeLens(PlayerOptics.charStats.composeLens(CharStatsOptics.hp)).set(0)
    val removeEnemies = GameStateOptics.EncounterLens.modify(pem => (pem._1, Set.empty, pem._3))

    "Given an active game" must {

      lazy val result = GameOperations.PostTurnValidation(activeGame)

      "Return a Right sided Either" in {
        assert(result.isRight)
      }
    }

    "Given a game with a dead player" must {
      lazy val result = GameOperations.PostTurnValidation(deadPlayerLens(activeGame))

      "Return a Left sided Either" in {
        assert(result.isLeft)
      }
    }

    "Given a game with a dead enemies" must {
      lazy val result = GameOperations.PostTurnValidation(removeEnemies(activeGame))

      "Return a Left sided Either" in {
        assert(result.isLeft)
      }
    }

  }

  "GameOperations.GameOverCheck" when {

    val activeGame = GameStateGenerator.staticGameState
    val deadPlayerLens = GameStateOptics.PlayerLens.composeLens(PlayerOptics.charStats.composeLens(CharStatsOptics.hp)).set(0)
    val removeEnemies = GameStateOptics.EncounterLens.modify(pem => (pem._1, Set.empty, pem._3))

    "Given an active game" must {

      lazy val result = GameOperations.GameOvercheck(activeGame)

      "Return a Right sided Either" in {
        assert(result.isRight)
      }
    }

    "Given a game with a dead player" must {
      lazy val result = GameOperations.GameOvercheck(deadPlayerLens(activeGame))

      "Return a Left sided Either" in {
        assert(result.isLeft)
      }
    }

    "Given a game with a dead enemies" must {
      lazy val result = GameOperations.GameOvercheck(removeEnemies(activeGame))

      "Return a Left sided Either" in {
        assert(result.isLeft)
      }
    }

  }

}
