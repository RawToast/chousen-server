package chousen.api.data

import monocle.{Lens, PLens}
import monocle.macros.GenLens

object PlayerOptics extends PlayerOptics

trait PlayerOptics {
  val PlayerCharStatsLens = GenLens[Player](_.stats)

  val SetPlayerStats = (s: Int, d: Int, i: Int, v: Int) => PlayerStrengthLens.modify(_ + s).andThen(PlayerDexterityLens.modify(_ + d))
    .andThen(PlayerVitalityLens.modify(_ + v).andThen(PlayerIntellectLens.modify(_ + i)))
  val PlayerHealthLens: PLens[Player, Player, Int, Int] = PlayerCharStatsLens.composeLens(CharStatsOptics.HpLens)
  val PlayerMaxHealthLens: PLens[Player, Player, Int, Int] = PlayerCharStatsLens.composeLens(CharStatsOptics.MaxHpLens)
  val PlayerStrengthLens: PLens[Player, Player, Int, Int] = PlayerCharStatsLens.composeLens(CharStatsOptics.StrengthLens)
  val PlayerDexterityLens: PLens[Player, Player, Int, Int] = PlayerCharStatsLens.composeLens(CharStatsOptics.DexterityLens)
  val PlayerIntellectLens: PLens[Player, Player, Int, Int] = PlayerCharStatsLens.composeLens(CharStatsOptics.IntellectLens)
  val PlayerVitalityLens: PLens[Player, Player, Int, Int] = PlayerCharStatsLens.composeLens(CharStatsOptics.VitalityLens)
  val PlayerSpeedLens: PLens[Player, Player, Int, Int] = PlayerCharStatsLens.composeLens(CharStatsOptics.SpeedLens)
  val PlayerStatusLens: Lens[Player, Seq[Status]] = GenLens[Player](_.status)
  val PlayerPositionLens = GenLens[Player](_.position)
}

object CharStatsOptics extends CharStatsOptics

trait CharStatsOptics {
  val DEFAULT = CharStats(100, 100)
  val DEFAULT_STAT = 8
  val DEFAULT_INTELLECT = DEFAULT_STAT

  // The GenLens methods are a macro for the manual creation of a Lens:
  // val currentHp = Lens[CharStats, Int](_.currentHp)((hp:Int) => s => s.copy(currentHp = hp))

  val HpLens = GenLens[CharStats](_.currentHp)
  val MaxHpLens = GenLens[CharStats](_.maxHp)

  val StrengthLens = GenLens[CharStats](_.strength)
  val DexterityLens = GenLens[CharStats](_.dexterity)
  val IntellectLens = GenLens[CharStats](_.intellect)
  val VitalityLens = GenLens[CharStats](_.vitality)
  val SpeedLens = GenLens[CharStats](_.speed)
}

object EnemyOptics extends EnemyOptics

trait EnemyOptics {
  val EnemyStats = GenLens[Enemy](_.stats)
  val EnemyPosition = GenLens[Enemy](_.position)
}

trait CharacterOptics extends PlayerOptics with CharStatsOptics with EnemyOptics