package chousen.api.data

import monocle.PLens
import monocle.macros.GenLens


object PlayerOptics {
  val PlayerCharStatsLens = GenLens[Player](_.stats)

  val SetPlayerStats = (s: Int, d: Int, i: Int, v: Int) => PlayerStrengthLens.modify(_ + s).andThen(PlayerDexterityLens.modify(_ + d))
    .andThen(PlayerVitalityLens.modify(_ + v).andThen(PlayerIntellectLens.modify(_ + i)))
  val PlayerHealthLens: PLens[Player, Player, Int, Int] = PlayerCharStatsLens.composeLens(CharStatsOptics.hp)
  val PlayerMaxHealthLens: PLens[Player, Player, Int, Int] = PlayerCharStatsLens.composeLens(CharStatsOptics.maxHp)
  val PlayerStrengthLens: PLens[Player, Player, Int, Int] = PlayerCharStatsLens.composeLens(CharStatsOptics.strength)
  val PlayerDexterityLens: PLens[Player, Player, Int, Int] = PlayerCharStatsLens.composeLens(CharStatsOptics.dexterity)
  val PlayerIntellectLens: PLens[Player, Player, Int, Int] = PlayerCharStatsLens.composeLens(CharStatsOptics.intellect)
  val PlayerVitalityLens: PLens[Player, Player, Int, Int] = PlayerCharStatsLens.composeLens(CharStatsOptics.vitality)
  val PlayerSpeedLens: PLens[Player, Player, Int, Int] = PlayerCharStatsLens.composeLens(CharStatsOptics.speed)
  val PlayerPositionLens = GenLens[Player](_.position)
}

object CharStatsOptics {
  val DEFAULT = CharStats(100, 100)
  val DEFAULT_STAT = 8
  val DEFAULT_INTELLECT = DEFAULT_STAT

  // The GenLens methods are a macro for the manual creation of a Lens:
  // val currentHp = Lens[CharStats, Int](_.currentHp)((hp:Int) => s => s.copy(currentHp = hp))

  val hp = GenLens[CharStats](_.currentHp)
  val maxHp = GenLens[CharStats](_.maxHp)

  val strength = GenLens[CharStats](_.strength)
  val dexterity = GenLens[CharStats](_.dexterity)
  val intellect = GenLens[CharStats](_.intellect)
  val vitality = GenLens[CharStats](_.vitality)
  val speed = GenLens[CharStats](_.speed)
}


object EnemyOptics {
  var charStats = GenLens[Enemy](_.stats)
  var position = GenLens[Enemy](_.position)
}