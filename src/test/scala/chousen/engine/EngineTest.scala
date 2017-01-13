package chousen.engine

import chousen.character.EnemyCharacter.baseCharacter
import chousen.character.{EnemyCharacter, FireBall}
import org.scalatest.{Matchers, WordSpec}

class EngineTest extends WordSpec with Matchers {

  "Engine" when {
    "Calculating physical damage" should {
      "Deal 8-13 damage to two base characters" in {

        @scala.annotation.tailrec
        def test(enemies: List[(EnemyCharacter, EnemyCharacter)]): List[(EnemyCharacter, EnemyCharacter)] = {
          val (e1: EnemyCharacter, e2: EnemyCharacter) = enemies.head

          val dmg = Engine.calcDamage(e1, e2)

          // See calculation: chousen/engine/Engine.scala:9
          // 2 equal characters will deal 8-13 damage
          dmg should be >= 8
          dmg should be <= 13
          val tail = enemies.tail
          if (tail.isEmpty) tail
          else test(tail)
        }

        val es = Range.inclusive(1, 100)
          .toList.map(_ => (baseCharacter, baseCharacter))
        val r = test(es)

        r should have size 0
      }
    }

    "Calculating magic damage" should {

      s"Fireball: 3-6 damage when using two base characters" in {

        val spell = new FireBall

        @scala.annotation.tailrec
        def test(enemies: List[(EnemyCharacter, EnemyCharacter)]): List[(EnemyCharacter, EnemyCharacter)] = {
          val (e1: EnemyCharacter, e2: EnemyCharacter) = enemies.head

          val dmg = Engine.calcMagic(spell, e1, e2)

          // See calculation: chousen/engine/Engine.scala:22
          // FireBall will deal 3-6 damage
          dmg should be >= 3
          dmg should be <= 6
          val tail = enemies.tail
          if (tail.isEmpty) tail
          else test(tail)
        }

        val es = Range.inclusive(1, 100)
          .toList.map(_ => (baseCharacter, baseCharacter))
        val r = test(es)

        r should have size 0
      }
    }
  }
}
