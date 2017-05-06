package chousen.game.core

import java.util.UUID

import chousen.api.data._
import chousen.util.LensUtil
import monocle._
import monocle.function.At
import monocle.macros.GenLens

object GameStateOptics {

  val DungeonLens = GenLens[GameState](_.dungeon)

  val PlayerLens = GenLens[GameState](_.player)
  val MessagesLens = GenLens[GameState](_.messages)

  val EncounterLens: Lens[GameState, (Player, Set[Enemy], Seq[GameMessage])] =
    LensUtil.triLens(PlayerLens,
      GenLens[GameState](_.dungeon.currentEncounter.enemies),
      MessagesLens)

  val DungeonTriLens: Lens[GameState, (Player, Dungeon, Seq[GameMessage])] =
    LensUtil.triLens(PlayerLens, DungeonLens, MessagesLens)

  def targettedLens(uuid: UUID): Lens[GameState, (Player, Option[Enemy], Seq[GameMessage])] = {
    import monocle.Iso

    val enemiesIso = Iso[Set[Enemy], Map[UUID, Enemy]] {
      _.foldLeft(Map.empty[UUID, Enemy])((m, e) => m + (e.id -> e))
    } {
      _.foldLeft(Set.empty[Enemy])((es, m) => es + m._2)
    }

    val enemiesLens: Lens[GameState, Set[Enemy]] = GenLens[GameState](_.dungeon.currentEncounter.enemies)

    LensUtil.triLens(GenLens[GameState](_.player),
      enemiesLens.composeLens(At.fromIso(enemiesIso).at(uuid)),
      GenLens[GameState](_.messages))
  }

}
