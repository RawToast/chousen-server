package chousen.cast

import java.util.UUID

import chousen.Peoples
import chousen.character.{EnemyCharacter, PlayerCharacter}
import api.data.CharStats
import org.scalatest.WordSpec

class EncounterOpsSpec extends WordSpec {

  object EncounterOps {}


  def speed10Char = CharStats(100, 100, speed = 10)

  def speed8Char = CharStats(100, 100, speed = 8)

  "Peoples" when {

    val speed10Player = PlayerCharacter("Speed 10", UUID.randomUUID(), speed10Char)(position = 0)
    val enemy = EnemyCharacter("Enemy", UUID.randomUUID(), speed8Char)(position = 0)

    "provided with a fast player and a slow enemy" should {
      val p = Peoples.init(speed10Player, Set(enemy))


      "Set the fast player to be the active character" in {
        assert(p.active == speed10Player)
      }

      "Set the slow enemy's position to 80" in {
        assert(p.enemies.head.position == 80)
      }

      "Set the fast player's position to 100" in {
        assert(p.player.position == 100)
        assert(p.active.position == 100)
      }

    }
  }

  "Two actors" when {

    // Note that the enemy is closer to their next turn
    val bc1 = PlayerCharacter("Player", UUID.randomUUID(), speed10Char)(position = 0)
    val bc2 = EnemyCharacter("Enemy", UUID.randomUUID(), speed10Char)(position = 50)
    val cast = Peoples.init(bc1, Set(bc2))

    "initialised with equal speeds and different positions" should {
      "Set the fast player to be the actor with a higher position" in {
        assert(cast.active == bc2)
      }

      "Set the lower position character's positionto 50" in {
        assert(cast.player.position == 50)
      }

      "Set the higher position character's position to 100" in {
        assert(cast.active.position == 100)
        assert(cast.enemies.head.position == 100)
      }
    }

    val newCast = cast.changeTurn
    "it is the end of the turn" should {

      "the current actor is in the cast" in {
        assert(newCast.cast.contains(newCast.active))
      }

      "the previous actor is still in the cast" in {
        assert(newCast.cast.contains(cast.active))
      }

      "change the current actor" in {

        assert(cast.cast.size == newCast.cast.size)
        assert(cast.active !== newCast.active)
      }
    }

    // Player will have 111, enemy 110 so player goes first.

    val player = PlayerCharacter("Player", UUID.randomUUID(), speed10Char)(position = 91)
    val enemy = EnemyCharacter("Enemy", UUID.randomUUID(), speed10Char)(position = 90)
    val preCast = Peoples.init(player, Set(enemy))
    val turnOneCast = preCast.changeTurn

    "both reach the position goal" should {
      "the actor with the highest position takes precedence" in assert(preCast.active === player)

      "the faster actor's position is reset" in {
        assert(preCast.active.position == 101)
        assert(preCast.player.position == 101)
        assert(preCast.enemies.head.position == 100)
      }

      "the other character retains their position" in assert(preCast.enemies.head.position == 100)
    }

    "one player has already met the goal" should {
      "the actor with position > 100 is active" in {
        assert(turnOneCast.active == enemy)
        assert(turnOneCast.enemies.head.position == 100)
      }

      "the other actors position does not change" in {
        assert(turnOneCast.player.position == 1)
      }
    }
  }

  "both players reach the goal, with equal position and speed" should {
    // Player will have 111, enemy 110 so player goes first.

    val player = PlayerCharacter("Player", UUID.randomUUID(), speed8Char)()
    val enemy = EnemyCharacter.baseCharacter
    val turnOneCast = Peoples.init(player, Set(enemy))
    val turnTwoCast = turnOneCast.changeTurn
    val turnThreeCast = turnTwoCast.changeTurn
    val turnFourCast = turnThreeCast.changeTurn

    "have a different actor on the 1st/2nd turn" in assert(turnOneCast.active.name != turnTwoCast.active.name)
    "have a different actor on the 3rd/4th turn" in assert(turnThreeCast.active.name != turnFourCast.active.name)

    "have a different actor on each following turn" in {

      Range.inclusive(0, 5).foreach {(_:Int) =>

        val player = PlayerCharacter("Player", UUID.randomUUID(), speed8Char)()
        val enemy = EnemyCharacter.baseCharacter
        val turnOneCast = Peoples.init(player, Set(enemy))
        val turnTwoCast = turnOneCast.changeTurn
        val turnThreeCast = turnTwoCast.changeTurn
        val turnFourCast = turnThreeCast.changeTurn

        assert(turnOneCast.active.name != turnTwoCast.active.name)
        assert(turnThreeCast.active.name != turnFourCast.active.name)
      }

    }
  }


  "Three actors" when {

    // Note that expected order for 4 turns:
    // Calc x5 turns = (FP: 100, E: 65 SE: 50) - FP's turn
    // Calc x9 turns = (FP: 80, E: 105 SE: 90) - E's turn
    // Calc x10 turns = (FP: 100, E: 15 SE: 100) - no turn, another run will be done so 11 runs:
    // Calc x11 turns = (FP: 120, E: 25 SE: 110) - FP's turn
    // Calc x12 turns = (FP: 40, E: 35 SE: 20) - SE's turn

    val speed20Char = CharStats(100, 100, speed = 20)

    // When two players with the same position score reach the goal, the faster actor is favoured
    val fastestActor = PlayerCharacter("Fastest Actor", UUID.randomUUID(), speed20Char)(position = 0)
    val averageActor = EnemyCharacter("Average Actor", UUID.randomUUID(), speed10Char)(position = 15)
    val slowestActor = EnemyCharacter("SLowest Actor ", UUID.randomUUID(), speed10Char)(position = 0)

    //val preCast =
    val turnOneCast = Peoples.init(fastestActor, Set(averageActor, slowestActor))
    val turnTwoCast = turnOneCast.changeTurn
    val turnThreeCast = turnTwoCast.changeTurn
    val turnFourCast = turnThreeCast.changeTurn

    "it is the end of the first turn" should {
      "be the turn of the fastest actor" in assert(turnOneCast.active == fastestActor)
    }

    "it is the end of the second turn" should {
      "be the turn of the average actor" in {
        assert(turnTwoCast.active == averageActor)
        assert(turnTwoCast.active.position == 105)
      }
    }

    "it is the end of the third turn" should {
      "be the turn of the fastest actor" in assert(turnThreeCast.active == fastestActor)
      "the active actor's position not be reset" in assert(turnThreeCast.active.position == 100)
    }

    "it is the end of the forth turn" should {
      "be the turn of the slowest actor" in assert(turnFourCast.active == slowestActor)
      "the active actor's position should not be reset" in assert(turnFourCast.active.position == 100)
    }
  }
}
