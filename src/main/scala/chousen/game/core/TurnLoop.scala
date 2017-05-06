package chousen.game.core

import chousen.api.data.GameState

object GameTurnLoop extends TurnLoop(GameOperations.UpdateUntilPlayerIsActive,
  GameOperations.PreTurnValidation, GameOperations.PostTurnValidation, GameOperations.GameOvercheck)


abstract class TurnLoop(takeEnemyTurns: GameOperation, preTurnValidation: ForkedGameOption,
                        postTurnValidation: ForkedGameOption, gameOverCheck: ForkedGameOption) {

  def takeTurn(gameState: GameState, playerInput: GameOperation): GameState = {

    // Using an Either to prevent actions from taking place when no action should be processable
    // e.g. If the player is dead or the enemies are dead, then no more actions should complete -- as
    // long as the GameState is on the Left side of the Either.
    val newState: Either[GameState, GameState] = for {
      checkedGame <- preTurnValidation(gameState)
      postInput <- playerInput.andThen(postTurnValidation)(checkedGame)
      postEnemy = takeEnemyTurns(postInput)
      postCheck2 <- gameOverCheck(postEnemy)
    } yield postCheck2


    // Return the state as is
    newState match {
      case Right(gs) => gs
      case Left(gs) => gs
    }
  }
}


object GameOperations extends GameOperations(GameOps)

abstract class GameOperations(gameops: GameOps) {
  val UpdateUntilPlayerIsActive: GameOperation =
    GameStateOptics.EncounterLens.modify(gameops.updateUntilPlayerIsActive)

  val PreTurnValidation: ForkedGameOption = gs => {
    val isActive = GameStateOptics.EncounterLens.get _ andThen gameops.isGameActive
    if (isActive(gs)) Right(gs)
    else Left(gs)
  }

  val PostTurnValidation: ForkedGameOption = gs => {
    val isActive = GameStateOptics.EncounterLens.get _ andThen gameops.isGameActive
    if (isActive(gs)) Right(gs)
    else Left(gs)
  }


  val GameOvercheck: ForkedGameOption = gs => {
    val isActive = GameStateOptics.EncounterLens.get _ andThen gameops.isGameActive
    if (isActive(gs)) Right(gs)
    else Left(gs)
  }
}