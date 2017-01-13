package chousen

trait Nameable {
  val name: String
}

case class CharStats(maxHp: Int, currentHp: Int,
                     override val strength: Int = 8,
                     override val dexterity: Int = 8,
                     override val intellect: Int = 8,
                     override val vitality: Int = 8,
                     override val speed: Int = 8) extends BaseStats

object CharStats {

  import monocle.Lens

  val DEFAULT = CharStats(100, 100)

  val company = Lens[CharStats, Int](_.currentHp)(c => e => e.copy(currentHp = c))

  val currentHp = Lens[CharStats, Int](_.currentHp)((hp: Int) => s => s.copy(currentHp = hp))
  val strength = Lens[CharStats, Int](_.strength)(str => s => s.copy(strength = str))
  val dexterity = Lens[CharStats, Int](_.dexterity)(dex => s => s.copy(dexterity = dex))
  val intellect = Lens[CharStats, Int](_.intellect)(int => s => s.copy(intellect = int))
  val vitality = Lens[CharStats, Int](_.vitality)(vit => s => s.copy(vitality = vit))
  val speed = Lens[CharStats, Int](_.speed)(spd => s => s.copy(speed = spd))
}

trait Stats {
  val stats: CharStats
}

trait BaseStats {

  val maxHp: Int
  val currentHp: Int

  val strength: Int
  val dexterity: Int
  val intellect: Int
  val vitality: Int

  val speed: Int
}

object BaseStats {
  val DEFAULT_STAT = 8
  val DEFAULT_INTELLECT = DEFAULT_STAT
}

trait CanLevel {
  stats: BaseStats =>
  def levelUp(): BaseStats
}


trait Options[T] {

  val items: List[T]

  lazy val options: Map[String, T] = items.foldLeft(Map.empty[Int, T]) {
    (m, bc: T) =>
      if (m.isEmpty) m + ((1, bc))
      else m.+((m.keySet.max + 1, bc))
  }.foldLeft(Map.empty[String, T])((m, bc) => m.+((bc._1.toString, bc._2)))

  lazy val optionString = options.toSeq.sortBy(_._1).map(kv => s"[${kv._1}]:${kv._2} ").mkString
}