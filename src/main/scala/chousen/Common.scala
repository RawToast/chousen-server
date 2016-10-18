package chousen

trait Nameable {
  val name: String
}

trait Stats {

  val maxHp: Int
  val currentHp: Int

  val strength: Int = Stats.DEFAULT_STAT
  val dexterity: Int = Stats.DEFAULT_STAT
  val intellect: Int = Stats.DEFAULT_INTELLECT
  val vitality: Int = Stats.DEFAULT_STAT

  val speed: Int = Stats.DEFAULT_STAT
}
object Stats {
  val DEFAULT_STAT = 8
  val DEFAULT_INTELLECT = DEFAULT_STAT
}

trait CanLevel {
  stats: Stats =>
  def levelUp(): Stats
}