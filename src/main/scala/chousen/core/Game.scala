package chousen.core

import java.util.UUID

import api.data.{GameMessage, GameResponse}
import chousen.cards.DeckManager
import chousen.character.{BaseCharacter, EnemyCharacter, PlayerCharacter}
import chousen.{Cast, Peoples, core}
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

  val messages: Lens[Game, Seq[GameMessage]] = GenLens[Game](_.messages)

  val currentEnemies: PLens[Game, Game, Set[BaseCharacter], Set[BaseCharacter]] =
    Game.dungeon composeLens Dungeon.current composeLens Encounter.enemies

  val refreshFromCast: (Cast) => (Game) => Game = (p: Cast) => {
    Game.player.set(p.player) compose currentEnemies.set(p.enemies)
  }

  val _encounterLens: Lens[Game, (PlayerCharacter, Set[BaseCharacter])] = mergeLens(Game.player, Game.currentEnemies)

  val ultLens: Lens[Game, (PlayerCharacter, Set[BaseCharacter], Seq[GameMessage])] = triLens(Game.player, Game.currentEnemies, Game.messages)


  private def mergeLens[S, A, B](lsa : Lens[S, A], lsb : Lens[S, B]) : Lens[S, (A, B)] =
    Lens.apply[S, (A, B)](s => (lsa.get(s), lsb.get(s)))(t => lsa.set(t._1).andThen(lsb.set(t._2)))

  private def triLens[S, A, B, C](lsa : Lens[S, A], lsb : Lens[S, B], lsc: Lens[S, C]) : Lens[S, (A, B, C)] =
    Lens.apply[S, (A, B, C)](s => (lsa.get(s), lsb.get(s), lsc.get(s)))(t => lsa.set(t._1).andThen(lsb.set(t._2)).andThen(lsc.set(t._3)))
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