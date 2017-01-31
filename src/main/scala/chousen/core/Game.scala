package chousen.core

import java.util.UUID

import api.data.{GameMessage, GameResponse}
import chousen.cards.DeckManager
import chousen.character.{BaseCharacter, EnemyCharacter, PlayerCharacter}
import chousen.{Peoples, core}
import monocle.{Lens, PLens}
import monocle.macros.GenLens



trait Actionable[T] {
  def affect(g:T): T
}

case class Game(id: UUID, player: PlayerCharacter, deckManager: DeckManager,
                quest: Dungeon, messages: Seq[GameMessage] = Seq.empty)

object Game {
  def create(p: PlayerCharacter, dm: DeckManager, d: Dungeon, msg: Seq[GameMessage] = Seq.empty): Game = {
    Game(UUID.randomUUID(), p, dm, d, msg)
  }

  def toResponse(game: Game): GameResponse = {
    import api.data.Implicits._
    GameResponse(game.id, game.player, game.deckManager, game.quest, game.messages)
  }

  val player: Lens[Game, PlayerCharacter] = GenLens[Game](_.player)

  val dungeon: Lens[Game, Dungeon] = GenLens[Game](_.quest)

  val currentEnemies: PLens[Game, Game, Set[BaseCharacter], Set[BaseCharacter]] =
    Game.dungeon composeLens Dungeon.current composeLens Encounter.enemies

  val refreshFromPeoples: (Peoples) => (Game) => Game = (p: Peoples) => {
    Game.player.set(p.player) compose currentEnemies.set(p.enemies)
  }

}

case class Dungeon(encounters: List[Encounter]) {
  val isComplete = encounters.isEmpty

  val isNotComplete = encounters.nonEmpty

  def nextEncounter = encounters.headOption

  def progress = core.Dungeon(encounters.tail)

  lazy val current: Encounter = encounters.head
}

object Dungeon {
  import monocle.Lens

  val current: Lens[Dungeon, Encounter] =
    Lens[Dungeon, Encounter](_.current)(enc => dng => dng.copy(enc :: dng.encounters.tail))
}


case class Encounter(enemies: Set[BaseCharacter]) {
  def +(enemyCharacter: BaseCharacter) = this.copy(enemies + enemyCharacter)

  def ++(encounter: Encounter) = this.copy(this.enemies ++ encounter.enemies)

  override def toString = {
    if (this.isSingleEnemy) s"A ${this.enemies.head} appears"
    else s"${this.enemies.map(en => en.name).mkString(", ")} appears"
  }

  private def isSingleEnemy = this.enemies.size == 1
}

object Encounter {
  def create(enemyCharacter: EnemyCharacter) = Encounter(Set(enemyCharacter))
  import monocle.macros.GenLens

  val enemies : Lens[Encounter, Set[BaseCharacter]] = GenLens[Encounter](_.enemies)

}