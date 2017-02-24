package chousen.cast

import java.util.UUID

import api.data.{CharStats, GameMessage}
import chousen.Peoples
import chousen.character.{BaseCharacter, EnemyCharacter, PlayerCharacter}
import org.scalatest.WordSpec

class EncounterSpec extends WordSpec {

  def speed10Char = CharStats(100, 100, speed = 10)

  def speed8Char = CharStats(100, 100, speed = 8)


  object EncounterOps {

  }

  "EncounterOps" when {
    val speed10Player = PlayerCharacter("Speed 10", UUID.randomUUID(), speed10Char)(position = 0)
    val enemy = EnemyCharacter("Enemy", UUID.randomUUID(), speed8Char)(position = 0)


    "initialised with a fast player and a slow enemy" should {

      val (pc: PlayerCharacter, es: Set[BaseCharacter], msgs: Seq[GameMessage] = EncounterOps.update(speed10Player, Set(enemy), Seq.empty[GameMessage])



      "Set the fast player to be the active character" in {
        ???
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
}
