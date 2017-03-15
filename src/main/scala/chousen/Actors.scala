package chousen

import chousen.cards.DeckManager
import chousen.character.{BaseCharacter, EnemyCharacter, PlayerCharacter}
import chousen.engine.State

import scala.annotation.tailrec
import scala.util.Random

trait Scene {
  val player: PlayerCharacter
  val enemies: Set[BaseCharacter]
}

trait Cast extends Scene {

  val cast: Set[BaseCharacter]

  val player: PlayerCharacter

  val enemies: Set[BaseCharacter]

  val state: State

  def active: BaseCharacter

  def actor = active

  def changeTurn: Cast

  def fullCastWithoutPlayer = cast - player

  def hasEnemies: Boolean

  def isPlayerActive = player == active

  def takeTurn(dm: DeckManager): (Cast, DeckManager) = this.active match {
    case player: PlayerCharacter => player.playerInput(this, dm)
    case enemy: EnemyCharacter => (enemy.attack(Set(this.player), Option(this.fullCastWithoutPlayer)), dm)
  }

  def postAttackState: State = {
    val postActionCast = removeDeadActors()
    State.createFromActors(postActionCast)
  }

  def removeDeadActors(): Cast
}

/**
  * Safer Actors implementation, guarantees the existence of a Player.
  */
object Peoples {

  def init(scene: Scene): Peoples = init(scene.player, scene.enemies)

  def init(player: PlayerCharacter, enemies: Set[BaseCharacter]): Peoples = {

    @tailrec
    def calculateActiveCast(p: PlayerCharacter, es: Set[BaseCharacter]): Peoples = {

      case class Actorz(player: PlayerCharacter, enemies: Set[BaseCharacter]) {

        def actorsWith(filter: (BaseCharacter) => Boolean): (Option[PlayerCharacter], Set[BaseCharacter]) = {
          val po = if (filter(player)) Some(player) else None
          (po, enemies.filter(filter))
        }

        def numActorsWith(f: (BaseCharacter) => Boolean): Int = {
          val (po, es) = actorsWith(f)

          es.size + {
            if (po.nonEmpty) 1 else 0
          }
        }

        def updatePositions(): Actorz = {
          Actorz(player.updatePosition, enemies.map(_.updatePosition))
        }
      }

      lazy val nextActorz: Actorz = Actorz(p, es).updatePositions()

      lazy val actorsWithPosition: (Option[PlayerCharacter], Set[BaseCharacter]) = nextActorz.actorsWith(_.hasPosition)
      lazy val actorsHadPosition = nextActorz.actorsWith(_.hadPosition)

      def count(aw: (Option[PlayerCharacter], Set[BaseCharacter])) = {
        val p = aw._1
        val es = aw._2

        p match {
          case Some(pc) => 1 + es.size
          case None => es.size
        }
      }

      nextActorz.numActorsWith(_.hasPosition) match {
        case 0 => calculateActiveCast(nextActorz.player, nextActorz.enemies) //(nextPeeps)

        case 1 =>

          val srtE = nextActorz.enemies.toList.sortBy(f => -f.position)

          if (nextActorz.player.position > srtE.head.position) {
            Peoples(nextActorz.player, nextActorz.enemies)
          } else {
            Peoples(nextActorz.player, nextActorz.enemies)
          }
        case _ =>
          val awp2: Set[BaseCharacter] = actorsWithPosition._1 match {
            case Some(bc) => actorsWithPosition._2 + bc
            case None => actorsWithPosition._2
          }

          val awp = awp2.toList.sortBy(f => -f.position)

          val A1 = awp.head
          val A2 = awp.tail.head

          if (A1.position > A2.position) {
            Peoples(nextActorz.player, nextActorz.enemies)
          } else {
            count(actorsHadPosition) match {
              case 0 => calculateActiveCast(nextActorz.player, nextActorz.enemies)
              case 1 => Peoples(p, es)
              case _ =>
                val topSpeedCount = count(nextActorz.actorsWith(bc => bc.stats.speed == {
                  val fastestEnemySpeed = es.maxBy(_.stats.speed).stats.speed
                  actorsWithPosition._1 match {
                    case Some(pc) => if (pc.stats.speed > fastestEnemySpeed) pc.stats.speed else fastestEnemySpeed
                    case None => fastestEnemySpeed
                  }
                }))
                if (topSpeedCount == 1) calculateActiveCast(nextActorz.player, nextActorz.enemies) //(nextPeeps)
                else Peoples(nextActorz.player, nextActorz.enemies)
            }
          }
      }
    }

    calculateActiveCast(player, enemies)
  }
}

case class Peoples(player: PlayerCharacter, enemies: Set[BaseCharacter]) extends Cast {

  override lazy val cast: Set[BaseCharacter] = enemies + player

  override lazy val state: State = State(player.isAlive, this)

  override def hasEnemies: Boolean = enemies.nonEmpty

  def update(bc: BaseCharacter): Peoples = bc match {
    case p: PlayerCharacter =>
      Peoples(p, enemies)
    case b: BaseCharacter =>
      Peoples(player, Set(b) ++ enemies)
  }

  def changeTurn: Peoples = calculateActiveCast._2

  private def updatePositions: Peoples = {
    val nnp = player.updatePosition
    val nem = enemies.map(_.updatePosition)

    Peoples(nnp, nem)
  }

  private def resetBaseCharacter(bc: BaseCharacter) = bc.resetPosition


  // Very simple turn change
  override def active: BaseCharacter = {
    def calculateActive: BaseCharacter = this.calculateActiveCast._1

    calculateActive
  }

  final private lazy val calculateActiveCast: (BaseCharacter, Peoples) = {

    val currentWithPosition = this.cast.filter(_.hasPosition)

    currentWithPosition.size match {
      case 0 => this.updatePositions.calculateActiveCast

      case 1 =>
        val srtE = this.enemies.toList.sortBy(f => -f.position)

        if (this.player.position > srtE.head.position) {
          (this.player, this.update(resetBaseCharacter(this.player)))
        } else {
          val ne = srtE.head
          (ne, this.update(resetBaseCharacter(ne)))
        }
      case _ => {
        val awp: List[BaseCharacter] = currentWithPosition.toList.sortBy(f => -f.position)

        val A1 = awp.head
        val A2 = awp.tail.head

        if (A1.position > A2.position) {
          val pp = A1.resetPosition

          if (A1.isInstanceOf[PlayerCharacter]) (this.player, this.update(pp))
          else (this.enemies.find(p => p == A1).get, this.update(pp))
        } else {
          val sortBySpd = currentWithPosition.
            filter(bc => bc.position == A1.position).
            toList.sortBy(bc => -bc.stats.speed)


          val spd = sortBySpd.filter(bc => bc.stats.speed == sortBySpd.head.stats.speed)

          spd.size match {
            case 1 => {
              val pp = spd.head.resetPosition
              spd.head match {
                case _: PlayerCharacter => (this.player, this.update(pp))
                case _ => (this.enemies.find(p => p == spd.head).get, this.update(pp))
              }
            }
            case _ =>
              val chosenOne = Random.shuffle(spd).head
              (chosenOne, this.update(resetBaseCharacter(chosenOne)))
              val r = Random.setSeed(System.currentTimeMillis)
              
              (chosenOne, this)
          }
        }
      }
    }
  }


  override def removeDeadActors(): Cast = {
    val (alive: Set[BaseCharacter], dead: Set[BaseCharacter]) = this.enemies.partition(cm => cm.stats.currentHp > 0)
    dead.foreach(cm => exclaim(cm.deathMessage))
    if (!player.isAlive) exclaim(player.deathMessage)

    Peoples(player = player, enemies = alive)
  }

}
