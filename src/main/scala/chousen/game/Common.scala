package chousen.game

import chousen.api.data.CharStats

trait Nameable {
  val name: String
}


trait Stats {
  val stats: CharStats
}

trait CanLevel {
  stats: CharStats =>
  def levelUp(): CharStats
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