package chousen.game.core

import java.util.UUID

import chousen.api.data._
import chousen.util.LensUtil
import monocle.function.At
import monocle.macros.GenLens
import monocle.{Lens, _}

object GameStateOptics extends GameStateOptics

trait GameStateOptics {

  val DungeonLens = GenLens[GameState](_.dungeon)

  val PlayerLens = GenLens[GameState](_.player)
  val MessagesLens = GenLens[GameState](_.messages)

  val HandLens = GenLens[GameState](_.cards.hand)
  val DiscardLens = GenLens[GameState](_.cards.discard)


  val EncounterLens: Lens[GameState, (Player, Set[Enemy], Seq[GameMessage])] =
    LensUtil.triLens(PlayerLens,
      GenLens[GameState](_.dungeon.currentEncounter.enemies),
      MessagesLens)

  val DungeonTriLens: Lens[GameState, (Player, Dungeon, Seq[GameMessage])] =
    LensUtil.triLens(PlayerLens, DungeonLens, MessagesLens)


  def targettedLens(uuid: UUID): Lens[GameState, (Player, Option[Enemy], Seq[GameMessage])] = {

    val enemiesLens: Lens[GameState, Set[Enemy]] = GenLens[GameState](_.dungeon.currentEncounter.enemies)

    LensUtil.triLens(PlayerLens,
      enemiesLens.composeLens(At.fromIso(enemiesIso).at(uuid)),
      MessagesLens)
  }


  private def enemiesIso = Iso[Set[Enemy], Map[UUID, Enemy]] { (ex: Set[Enemy]) =>
    ex.foldLeft(Map.empty[UUID, Enemy])((m, e) => m + (e.id -> e))
  } {
    (uToEnemy: Map[UUID, Enemy]) => uToEnemy.foldLeft(Set.empty[Enemy])((es, m) => es + m._2)
  }

}
