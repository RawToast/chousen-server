package chousen

import chousen.character.{BaseCharacter, EnemyCharacter, PlayerCharacter}
import chousen.engine.State

import scala.util.Random


trait Cast {

  val cast: Set[BaseCharacter]

  val active: BaseCharacter

  val player: PlayerCharacter

  val state: State

  val actor = active

  def changeTurn: Cast

  def fullCast = cast // TODO remove

  def fullCastWithoutPlayer = cast - player

  def hasEnemies: Boolean

  val isPlayerActive = player == active

}

/**
  * Safer Actors implementation, guarantees the existence of a Player.
  */
case class Peoples(player: PlayerCharacter, enemies: Set[BaseCharacter]) extends Cast {
  override val cast: Set[BaseCharacter] = enemies + player
  override val state: State = State(player.isAlive, this)

  override def hasEnemies: Boolean = enemies.nonEmpty

  def update(bc: BaseCharacter): Peoples = bc match {
    case p: PlayerCharacter => this.copy(p)
    case b: BaseCharacter => this.copy(enemies = enemies + b)
  }

  def changeTurn: Peoples = Peoples(player.updatePosition, enemies.map(_.updatePosition))

  // Very simple turn change
  override lazy val active: BaseCharacter = {

    def calculateActive(peoples: Peoples): BaseCharacter = {
      def returnBc(bc:BaseCharacter) = bc.resetPosition

      val nextPeeps = peoples.changeTurn

      def actorsWith: ((BaseCharacter) => Boolean) => Set[BaseCharacter] =
        nextPeeps.cast.filter(_: (BaseCharacter) => Boolean)

      def numActorsWith(f: (BaseCharacter) => Boolean) =
        actorsWith(f)
          .size

      lazy val actorsWithPosition = actorsWith(_.hasPosition)
      lazy val actorsHadPosition = actorsWith(_.hadPosition)


      actorsWithPosition.size match {
        case 0 => calculateActive(nextPeeps)

        case 1 =>
          val activeActor: BaseCharacter = nextPeeps.cast.maxBy(bc => bc.position)
          returnBc(activeActor)

        case _ =>
          val awp: List[BaseCharacter] = actorsWithPosition.toList.sortBy(f => -f.position)
          val a1 = awp.head
          val a2 = awp.tail.head

          if (a1.position > a2.position) {
            returnBc(a1)
          } else {
            actorsHadPosition.size match {
              case 0 => calculateActive(nextPeeps)
              case 1 =>
                returnBc(actorsHadPosition.head)
              case _ =>
                val topSpeedCount = numActorsWith(bc => bc.stats.speed == actorsWithPosition.maxBy(_.stats.speed).stats.speed)
                if (topSpeedCount == 1) calculateActive(nextPeeps)
                else {
                  val chosenOne = Random.shuffle(actorsWithPosition).head
                  returnBc(chosenOne)
                }
            }
          }
      }
    }

    calculateActive(this)
  }

}


case class Actors(override val actor: BaseCharacter, cast: Set[BaseCharacter]) extends Cast {

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

  override def fullCast = cast + actor

  override def fullCastWithoutPlayer = cast + actor - player

  val player: PlayerCharacter = {
    val x = cast.find(bc => bc.isPlayer).getOrElse(actor)

    PlayerCharacter(x.name, x.stats)(x.position)
  }

  override def hasEnemies = !actor.isPlayer || cast.exists(!_.isPlayer)

  override val active: BaseCharacter = actor
  override val state: State = State.createFromActors(this)
}

