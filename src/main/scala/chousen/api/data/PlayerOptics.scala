package chousen.api.data

import monocle.Lens
import monocle.macros.GenLens

object PlayerOptics extends PlayerOptics

trait PlayerOptics {
  val PlayerCharStatsLens = GenLens[Player](_.stats)

  val PlayerHealthLens: Lens[Player, Int] = PlayerCharStatsLens.composeLens(CharStatsOptics.HpLens)
  val PlayerMaxHealthLens: Lens[Player, Int] = PlayerCharStatsLens.composeLens(CharStatsOptics.MaxHpLens)
  val PlayerStrengthLens: Lens[Player, Int] = PlayerCharStatsLens.composeLens(CharStatsOptics.StrengthLens)
  val PlayerDexterityLens: Lens[Player, Int] = PlayerCharStatsLens.composeLens(CharStatsOptics.DexterityLens)
  val PlayerIntellectLens: Lens[Player, Int] = PlayerCharStatsLens.composeLens(CharStatsOptics.IntellectLens)
  val PlayerVitalityLens: Lens[Player, Int] = PlayerCharStatsLens.composeLens(CharStatsOptics.VitalityLens)
  val PlayerSpeedLens: Lens[Player, Int] = PlayerCharStatsLens.composeLens(CharStatsOptics.SpeedLens)
  val PlayerPositionLens: Lens[Player, Int] = GenLens[Player](_.position)

  val SetPlayerStats = (s: Int, d: Int, i: Int, v: Int) => PlayerStrengthLens.modify(_ + s).andThen(PlayerDexterityLens.modify(_ + d))
    .andThen(PlayerVitalityLens.modify(_ + v).andThen(PlayerIntellectLens.modify(_ + i)))
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