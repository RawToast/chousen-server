package chousen.api.data

import monocle.Lens
import monocle.macros.GenLens

object PlayerOptics extends PlayerOptics

trait PlayerOptics {
  val PlayerCharStatsLens: Lens[Player, CharStats] = GenLens[Player](_.stats)

  val PlayerHealthLens: Lens[Player, Int] = PlayerCharStatsLens.composeLens(CharStatsOptics.HpLens)
  val PlayerMaxHealthLens: Lens[Player, Int] = PlayerCharStatsLens.composeLens(CharStatsOptics.MaxHpLens)
  val PlayerStrengthLens: Lens[Player, Int] = PlayerCharStatsLens.composeLens(CharStatsOptics.StrengthLens)
  val PlayerDexterityLens: Lens[Player, Int] = PlayerCharStatsLens.composeLens(CharStatsOptics.DexterityLens)
  val PlayerIntellectLens: Lens[Player, Int] = PlayerCharStatsLens.composeLens(CharStatsOptics.IntellectLens)
  val PlayerVitalityLens: Lens[Player, Int] = PlayerCharStatsLens.composeLens(CharStatsOptics.VitalityLens)
  val PlayerSpeedLens: Lens[Player, Int] = PlayerCharStatsLens.composeLens(CharStatsOptics.SpeedLens)
  val PlayerPositionLens: Lens[Player, Int] = GenLens[Player](_.position)
  val PlayerStatusLens: Lens[Player, Seq[Status]] = GenLens[Player](_.status)
  val PlayerClassLens: Lens[Player, String] = GenLens[Player](_.className)
  val PlayerExperienceLens: Lens[Player, Experience] = GenLens[Player](_.experience)
  val PlayerCurrentExperienceLens: Lens[Player, Int] = GenLens[Player](_.experience.current)


  val SetPlayerStats: (Int, Int, Int, Int) => (Player) => Player =
    (s: Int, d: Int, i: Int, v: Int) => PlayerStrengthLens.modify(_ + s).andThen(PlayerDexterityLens.modify(_ + d))
    .andThen(PlayerVitalityLens.modify(_ + v).andThen(PlayerIntellectLens.modify(_ + i)))
}

object CharStatsOptics extends CharStatsOptics

trait CharStatsOptics {
  val DEFAULT: CharStats = CharStats(100, 100)
  val DEFAULT_STAT: Int = 8
  val DEFAULT_INTELLECT:Int  = DEFAULT_STAT

  // The GenLens methods are a macro for the manual creation of a Lens:
  // val currentHp = Lens[CharStats, Int](_.currentHp)((hp:Int) => s => s.copy(currentHp = hp))

  val HpLens: Lens[CharStats, Int] = GenLens[CharStats](_.currentHp)
  val MaxHpLens: Lens[CharStats, Int] = GenLens[CharStats](_.maxHp)

  val StrengthLens: Lens[CharStats, Int]= GenLens[CharStats](_.strength)
  val DexterityLens: Lens[CharStats, Int] = GenLens[CharStats](_.dexterity)
  val IntellectLens: Lens[CharStats, Int] = GenLens[CharStats](_.intellect)
  val VitalityLens : Lens[CharStats, Int]= GenLens[CharStats](_.vitality)
  val SpeedLens: Lens[CharStats, Int] = GenLens[CharStats](_.speed)
}

object EnemyOptics extends EnemyOptics

trait EnemyOptics {
  val EnemyStatsLens: Lens[Enemy, CharStats] = GenLens[Enemy](_.stats)
  val EnemyPosition: Lens[Enemy, Int] = GenLens[Enemy](_.position)
  val EnemyStatusLens: Lens[Enemy, Seq[Status]] = GenLens[Enemy](_.status)

  val EnemyHpLens: Lens[Enemy, Int] = GenLens[Enemy](_.stats.currentHp)
}

trait CharacterOptics extends PlayerOptics with CharStatsOptics with EnemyOptics