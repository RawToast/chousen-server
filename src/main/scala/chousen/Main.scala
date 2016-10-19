package chousen

import chousen.character.{BaseCharacter, EnemyCharacter, PlayerCharacter, PlayerChoice}
import chousen.engine.State

import scala.annotation.tailrec

object Main extends App {

  statement("Enter name: ")
  val name = requireCaseSensitivePlayerInput
  val player = PlayerCharacter(name)()
  val someEnemies: List[Set[BaseCharacter]] = List(Set(EnemyCharacter.yellowSlime(), EnemyCharacter.slime()),
    Set(EnemyCharacter.giantSlime()))

  GameLoop(name).loop(player, someEnemies)
}


case class GameLoop(playerName: String) {
  story(s"$playerName has entered the dungeon")
  story(s"It was dark and smelly")
  statement(s"Eventually $playerName finds a room with a chest!")

  def loop(p: BaseCharacter, enemies: List[Set[BaseCharacter]]) = {

    // Need to place these elsewhere
    implicit val convert = (b: BaseCharacter) => Set(b)

    break()
    @tailrec
    def innerLoop(actors: Actors): State = {

      // Move
      val postAttackActors = actors.actor match {
        case player: BaseCharacter with PlayerChoice => player.playerInput(actors)
        case enemy: BaseCharacter => enemy.attack(actors.player, Option(actors.fullCastWithoutPlayer))
      }

      val state = postAttackActors.postAttackState

      if (!state.playerAlive || !state.actors.hasEnemies) state
      else innerLoop(state.actors.changeTurn)
    }

    @tailrec
    def play(player: BaseCharacter, es: List[Set[BaseCharacter]]): List[Set[BaseCharacter]] = {
      if (es.nonEmpty) {
        // Get first enemy/enemies
        val encounter = es.head

        if (encounter.size == 1) exclaim(s"A ${encounter.head} appears")
        else {
          encounter.map(en => en.name)
          exclaim(s"${encounter.toString()} appears")
        }

        val actors = Actors(player, encounter)

        // Fight
        val result: State = innerLoop(actors.changeTurn)

        // Conclude
        if (result.playerAlive) play(result.actors.player, es.tail)
        else es
      } else Nil
    }

    if (play(p, enemies).nonEmpty) {
      exclaim("Game over")
    } else {
      story(s"$playerName found a wooden chest")
      story(s"$playerName opens the wooden chest")
      break()
      suspense(s"Unfortunately")
      story(s" it was a trap and $playerName died")
    }
  }
}


trait Action {
  char: BaseCharacter =>

  def complete(target: Set[BaseCharacter], bystanders: Option[Set[BaseCharacter]]): Actors
}