package chousen.core

import chousen.character.{BaseCharacter, EnemyCharacter}
import chousen.core



case class Dungeon(encounters: List[Encounter]) {
  val isComplete = encounters.isEmpty

  val isNotComplete = encounters.nonEmpty

  def nextEncounter = encounters.headOption

  def progress = core.Dungeon(encounters.tail)

  lazy val current: Encounter = encounters.head


//  override val update: Lens[Quest, Encounter] =
//    Lens[Quest, Encounter](_.encounters.head)(enc => dng => dng.copy(enc :: dng.encounters.tail))
}

object Dungeon {
  import monocle.Lens
  val update: Lens[Dungeon, Encounter] =
    Lens[Dungeon, Encounter](_.current)(enc => dng => dng.copy(enc :: dng.encounters.tail))
}


case class Encounter(enemies: Set[BaseCharacter]) {
  def +(enemyCharacter: BaseCharacter) = this.copy(enemies + enemyCharacter)

  def ++(encounter: Encounter) = this.copy(this.enemies ++ encounter.enemies)

  override def toString = {
    if (this.isSingleEnemy) s"A ${this.enemies.head} appears"
    else s"${this.enemies.map(en => en.name).mkString(", ")} appears"
  }

  private def isSingleEnemy = this.enemies.size == 1
}

object Encounter {
  def create(enemyCharacter: EnemyCharacter) = Encounter(Set(enemyCharacter))
}