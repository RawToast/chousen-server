package chousen

trait Nameable {
  val name: String
}

trait Stats {
  // Health Status
  val maxHp: Int
  val currentHp: Int

  val strength: Int = 8
  val dexterity: Int = 8
  val intellect: Int = 8
  val vitality: Int = 8

  val speed: Int = 8
}

trait CanLevel {
  stats: Stats =>
  def levelUp(): Stats
}