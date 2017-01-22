package chousen.character

import java.util.UUID

import chousen.data.CharStats
import chousen.engine.{ActionCalc, Engine}
import chousen.Peoples
import org.scalatest.{Matchers, WordSpec}


class FireBallTest extends WordSpec with Matchers {

  def speed10Char = CharStats.DEFAULT

  "FireBall" when {
    "used, but does not defeat any enemies" should {
      val spell: FireBall = new FireBall
      val calc: ActionCalc = Engine

      val player = PlayerCharacter("Bob", UUID.randomUUID(), speed10Char)(position = 1)
      val enemies: Set[BaseCharacter] = Set(EnemyCharacter.baseCharacter, EnemyCharacter.giantSlime)

      val res: Peoples = spell.complete(player, enemies, None)(calc)

      "make no changes to the size of the case" in {
        res.cast should have size 3
        res.player shouldBe player

        // Since enemies have taken damage, should not match
        res.enemies shouldNot be(enemies)
      }
    }
  }
}
