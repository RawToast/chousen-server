package chousen.character

import org.scalatest.{Matchers, WordSpec}

class EnemyCharacterTest extends WordSpec with Matchers{

  "Enemy Character" when {
    val enemy = EnemyCharacter.baseCharacter

    "Taking damage" should {
      val damage = 10
      val damagedEnemy = enemy.takeDamage(damage)
      "Reduce its hp by the amount of damage taken" in {
        damagedEnemy.stats.currentHp should be < enemy.stats.currentHp
        damagedEnemy.stats.currentHp should equal(enemy.stats.currentHp - damage)
      }
      "Not affect any other stat" in {
        damagedEnemy.stats.maxHp shouldEqual enemy.stats.maxHp
        damagedEnemy.stats.strength shouldEqual enemy.stats.strength
        damagedEnemy.stats.dexterity shouldEqual enemy.stats.dexterity
        damagedEnemy.stats.intellect shouldEqual enemy.stats.intellect
        damagedEnemy.stats.speed shouldEqual enemy.stats.speed
      }
    }
  }
}
