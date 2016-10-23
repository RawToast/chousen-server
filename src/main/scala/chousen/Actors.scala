package chousen

import chousen.character.{BaseCharacter, EnemyCharacter, PlayerCharacter}
import chousen.engine.State

import scala.util.Random

case class Actors(actor: BaseCharacter, cast: Set[BaseCharacter]) {

  // Very simple turn change
  def changeTurn: Actors = {

    def calculateTurn(fc: Set[BaseCharacter]): Actors = {

      val postCalcActors: Set[BaseCharacter] = fc.map {
        // Good argument that the position increase should be one my the BC class
        case p: PlayerCharacter => p.copy()(position = p.position + p.stats.speed)
        case e: EnemyCharacter => e.copy()(position = e.position + e.stats.speed)
      }

      def actorsWith: ((BaseCharacter) => Boolean) => Set[BaseCharacter] =
        postCalcActors.filter(_: (BaseCharacter) => Boolean)


      def numActorsWith(f: (BaseCharacter) => Boolean) =
        actorsWith(f)
          .size

      lazy val actorsWithPosition = actorsWith(_.hasPosition)
      lazy val actorsHadPosition = actorsWith(_.hadPosition)


      actorsWithPosition.size match {
        case 0 => calculateTurn(postCalcActors)
        case 1 =>
          val activeActor = postCalcActors.maxBy(bc => bc.position)
          Actors(activeActor.resetPosition, postCalcActors - activeActor)
        case _ =>
          val awp = actorsWithPosition.toList.sortBy(f => -f.position)

          if (awp.head.position > awp.tail.head.position) Actors(awp.head.resetPosition, postCalcActors - awp.head)
          else {
            actorsHadPosition.size match {
              case 0 => calculateTurn(postCalcActors)
              case 1 =>
                val actor = actorsHadPosition.head
                Actors(actor.resetPosition, postCalcActors - actor)
              case _ =>
                val topSpeedCount = numActorsWith(bc => bc.stats.speed == actorsWithPosition.maxBy(_.stats.speed).stats.speed)
                if (topSpeedCount == 1) calculateTurn(postCalcActors)
                else {
                  val chosenOne = Random.shuffle(actorsWithPosition).head
                  Actors(chosenOne.resetPosition, postCalcActors - chosenOne)
                }
            }
          }
      }
    }

    calculateTurn(fullCast)
  }

  def postAttackState: State = {
    val postActionCast = removeDeadActors()

    State.createFromActors(postActionCast)
  }

  def removeDeadActors(): Actors = {
    val (alive: Set[BaseCharacter], dead: Set[BaseCharacter]) = this.cast.partition(cm => cm.stats.currentHp > 0)
    dead.foreach(cm => exclaim(cm.deathMessage))
    Actors(this.actor, alive)
  }

  def fullCast = cast + actor

  def fullCastWithoutPlayer = cast + actor - player

  def player: BaseCharacter = cast.find(bc => bc.isPlayer).getOrElse(actor)

  def hasEnemies = !actor.isPlayer || cast.exists(!_.isPlayer)

  def newLead(actor: BaseCharacter) = Actors(actor.resetPosition, fullCast - actor)
}