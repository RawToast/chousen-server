package chousen.api.data

import monocle.macros.GenLens


object PlayerOptics {
  val charStats = GenLens[Player](_.stats)
}

object CharStatsOptics {
  val DEFAULT = CharStats(100, 100)
  val DEFAULT_STAT = 8
  val DEFAULT_INTELLECT = DEFAULT_STAT

  // The GenLens methods are a macro for the manual creation of a Lens:
  // val currentHp = Lens[CharStats, Int](_.currentHp)((hp:Int) => s => s.copy(currentHp = hp))

  val hp = GenLens[CharStats](_.currentHp)

  val strength = GenLens[CharStats](_.strength)
  val dexterity = GenLens[CharStats](_.dexterity)
  val intellect = GenLens[CharStats](_.intellect)
  val vitality = GenLens[CharStats](_.vitality)
  val speed = GenLens[CharStats](_.speed)
}


object EnemyOptics {
  var charStats = GenLens[Enemy](_.stats)
}