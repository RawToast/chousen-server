package chousen

import chousen.character.{EnemyCharacter, PlayerCharacter}
import org.scalatest.WordSpec

class ActorsTest extends WordSpec {

  "Two actors" when {

    "it is the end of the turn" should {

      // Note that the enemy is closer to their next turn
      val player = PlayerCharacter("Player", speed = 8)(position = 0)
      val enemy = EnemyCharacter("Enemy", 100, 100, speed = 8)(position = 50)

      val preActors = Actors(player, Set(enemy))
      val postActors = preActors.changeTurn

      "change the current actor" in {
        assert(preActors.actor !== postActors.actor)
      }

      "the current actor is removed from the cast" in {
        assert(!postActors.cast.contains(postActors.actor))
      }

      "the previous actor is now added to the cast" in {
        assert(postActors.cast.contains(preActors.actor))
      }
    }

    // Player will have 111, enemy 110 so player goes first.
    val player = PlayerCharacter("Player", speed = 10)(position = 91)
    val enemy = EnemyCharacter("Enemy", 100, 100, speed = 10)(position = 90)
    val preActors = Actors(player, Set(enemy))
    val turnOneActors = preActors.changeTurn

    "both reach the position goal" should {

      "the actor with the highest position takes precedence" in {
        // Player retains position
        assert(turnOneActors.actor === preActors.actor)
        assert(turnOneActors.actor === player)
        assert(turnOneActors.cast.contains(enemy))
      }

      // 1 rolls over
      "the faster actor's position is reset" in assert(turnOneActors.actor.position == 1)

      "the slower actor retains their position" in assert(turnOneActors.cast.head.position == 100)
    }

    "one player has already met the goal" should {
      val turnTwoActors = turnOneActors.changeTurn

      "the actor with position > 100 is active" in assert(turnTwoActors.actor == enemy)

      "the actors position is reset, but includes any movement" in assert(turnTwoActors.actor.position == 10)

      "the other actors position increases" in assert(turnTwoActors.cast.head.position == 11)
    }

    "both players reach the goal, with equal positon and speed" should {
      // Player will have 111, enemy 110 so player goes first.
      val player = PlayerCharacter("Player")()
      val enemy = EnemyCharacter.yellowSlime()
      val preActors = Actors(player, Set(enemy))
      val turnOneActors = preActors.changeTurn
      val turnTwoActors = turnOneActors.changeTurn

      "have a different actor on the 2nd turn" in assert(turnOneActors.actor.name != turnTwoActors.actor.name)
    }
  }


  "Three actors" when {

    // Note that expected order for 4 turns:
    // Calc x5 turns = (FP: 100, E: 65 SE: 50) - FP's turn
    // Calc x9 turns = (FP: 80, E: 105 SE: 90) - E's turn
    // Calc x10 turns = (FP: 100, E: 15 SE: 100) - no turn, another run will be done so 11 runs:
    // Calc x11 turns = (FP: 120, E: 25 SE: 110) - FP's turn
    // Calc x12 turns = (FP: 40, E: 35 SE: 20) - SE's turn

    // When two players with the same position score reach the goal, the faster actor is favoured
    val fastestActor = PlayerCharacter("Fastest Actor", speed = 20)(position = 0)
    val averageActor = EnemyCharacter("Average Actor", 100, 100, speed = 10)(position = 15)
    val slowestActor = EnemyCharacter("SLowest Actor ", 100, 100, speed = 10)(position = 0)

    val preActors = Actors(fastestActor, Set(averageActor, slowestActor))
    val turnOneActors = preActors.changeTurn
    val turnTwoActors = turnOneActors.changeTurn
    val turnThreeActors = turnTwoActors.changeTurn
    val turnFourActors = turnThreeActors.changeTurn

    "it is the end of the first turn" should {
      "be the turn of the fastest actor" in assert(turnOneActors.actor == fastestActor)
    }

    "it is the end of the second turn" should {
      "be the turn of the average actor" in assert(turnTwoActors.actor == averageActor)
    }

    "it is the end of the third turn" should {
      "be the turn of the fastest actor" in assert(turnThreeActors.actor == fastestActor)
      "the active actor's position should be partially reset" in assert(turnThreeActors.actor.position == 20)
    }

    "it is the end of the forth turn" should {
      "be the turn of the slowest actor" in assert(turnFourActors.actor == slowestActor)
      "the active actor's position should be partially reset" in assert(turnFourActors.actor.position == 20)
    }
  }
}
