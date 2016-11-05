package chousen.character

import cats.data.Xor
import chousen._
import chousen.cards.{Card, DeckManager}
import monocle.{Lens, PLens}

import scala.annotation.tailrec

sealed abstract class BaseCharacter
  extends Nameable with Stats with Attack {
  val isPlayer: Boolean
  val position: Int

  val hasPosition: Boolean = position >= 100
  val hadPosition: Boolean = hasPosition && ((position - stats.speed) >= 100)

  def takeDamage(damage: Int): BaseCharacter

  def deathMessage: String = {
    s"$name dies"
  }

  def resetPosition: BaseCharacter

  def updatePosition: BaseCharacter

  override def toString: String = name
}

case class PlayerCharacter(name: String, stats: CharStats)(override val position: Int = 0)
  extends BaseCharacter with PlayerChoice with Magic {

  override val spellBook = SpellBook.create.withSpell(new FireBall)

  override val isPlayer: Boolean = true

  val isAlive = stats.currentHp > 0

  override def takeDamage(damage: Int): BaseCharacter = {
    val currentHp = PlayerCharacter.currentHp.get(this)

    val currHp = currentHp - damage
    if (currHp <= 0) story(s"$name takes $damage")
    else statement(s"$name takes $damage and has ${currHp}HP left!")

    PlayerCharacter.currentHp.set(currHp)(this)
  }


  def resetPosition = copy()(position = this.position - 100)

  override def updatePosition: PlayerCharacter = copy()(position + stats.speed)

}

object PlayerCharacter {

  implicit def toBaseCharacter(pc: PlayerCharacter): BaseCharacter = pc

  val _stats = Lens[PlayerCharacter, CharStats](_.stats)((cs: CharStats) => p => p.copy(stats = cs)(position = p.position))

  val currentHp: PLens[PlayerCharacter, PlayerCharacter, Int, Int] = _stats composeLens CharStats.currentHp
  val str = _stats composeLens CharStats.strength
  val dex = _stats composeLens CharStats.dexterity
  val int = _stats composeLens CharStats.intellect
  val vit = _stats composeLens CharStats.vitality
  val spd = _stats composeLens CharStats.speed

  val posit = Lens[PlayerCharacter, Int](_.position)(pos => p => p.copy()(position = pos))
}

trait PlayerChoice {
  bc: BaseCharacter with Magic  =>

  @tailrec
  final def playerInput(actors: Cast, deckManager: DeckManager): (Cast, DeckManager) = {
    statement("[A]ttack [M]agic ")

    requirePlayerInput match {
      case "a" => (bc.attack(actors.cast, None), deckManager)
      case "m" => bc.useMagic(actors, deckManager)
      case _ => playerInput(actors, deckManager)
    }
  }
}

trait TopLevelBattleInput extends Choice {
  bc: BaseCharacter with Magic with UseCards =>

  @scala.annotation.tailrec
  final def takeInput(io: UserInput, a: Cast, d: DeckManager): Xor[Choice, (Cast, DeckManager)] = {
    io() match {
      case "a" => Xor.Right(bc.attack(a.cast, None), d)
      case "m" => Xor.Right(bc.useMagic(a, d))
      case "c" => use(a, d)
      case _ => takeInput(io, a, d)
    }
  }
}

trait Choice extends RecursiveChoice[Cast, DeckManager]

trait RecursiveChoice[A, D] {
  def takeInput(io: UserInput, a: A, d: D): Xor[RecursiveChoice[A, D], (A, D)]
}

trait UseCards {
  bc: BaseCharacter with TopLevelBattleInput =>

  def use(actors: Cast, dm: DeckManager): Xor[Choice, (Cast, DeckManager)] = {

    if (dm.hand.cards.isEmpty) {
      statement(s"Your hand is empty")
      bc.takeInput(PlayerInput, actors, dm)
    } else {

      def selectChoice(input: UserInput) = {
        val choices: Map[String, Card] = dm.hand.choices
        choices.get(input())
      }

      selectChoice(PlayerInput) match {
        case None => use(actors, dm)
        case Some(c: Card) =>
          val (_, na: Cast) = c.use(bc , actors)

          val ndm = dm.discard(c)

          Xor.Right(na, ndm)
      }
    }
  }
}

case class EnemyCharacter(name: String, stats: CharStats)(override val position: Int = 0)
  extends BaseCharacter {

  override val isPlayer: Boolean = false

  def this(name: String, hp: Int) = {
    this(name, CharStats(hp, hp))()
  }

  override def takeDamage(damage: Int): BaseCharacter = {

    EnemyCharacter.currentHp.modify(_ - damage)(this)
  }

  def resetPosition = copy()(position = position - 100)

  override def updatePosition: EnemyCharacter = copy()(position + stats.speed)
}


object EnemyCharacter {
  def create(name: String, maxHp: Int) = new EnemyCharacter(name, maxHp)

  def slime = EnemyCharacter("Slime", CharStats(10, 10, strength = 4, intellect = 4, speed = 7))()

  def yellowSlime = EnemyCharacter("Yellow Slime", CharStats(15, 15, intellect = 4, vitality = 2))()

  def giantSlime = EnemyCharacter("Giant Slime", CharStats(30, 30, strength = 18, intellect = 4, vitality = 12, speed = 6))()

  def baseCharacter = EnemyCharacter.create("Scoundrel", 100)

  def scoundrel = baseCharacter


  val _stats = Lens[EnemyCharacter, CharStats](_.stats) {
    (cs: CharStats) => (p: EnemyCharacter) => p.copy(stats = cs)(position = p.position)
  }

  val currentHp: PLens[EnemyCharacter, EnemyCharacter, Int, Int] = _stats composeLens CharStats.currentHp
  val str = _stats composeLens CharStats.strength
  val dex = _stats composeLens CharStats.dexterity
  val int = _stats composeLens CharStats.intellect
  val vit = _stats composeLens CharStats.vitality
  val spd = _stats composeLens CharStats.speed
}
