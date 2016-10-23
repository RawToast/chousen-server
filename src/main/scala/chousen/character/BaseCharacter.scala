package chousen.character

import cats.data.Xor
import chousen._
import chousen.cards.DeckManager
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

  override def toString: String = name
}

case class PlayerCharacter(name: String, stats: CharStats)(override val position: Int = 0)
  extends BaseCharacter with PlayerChoice with Magic with UseCards {

  override val spellBook = SpellBook.create.withSpell(new FireBall)

  override val isPlayer: Boolean = true

  override def takeDamage(damage: Int): BaseCharacter = {
    val currentHp = PlayerCharacter.currentHp.get(this)

    val currHp = currentHp - damage
    if (currHp <= 0) story(s"$name takes $damage")
    else statement(s"$name takes $damage and has ${currHp}HP left!")

    PlayerCharacter.currentHp.set(currHp)(this)
  }


  def resetPosition = copy()(position = this.position - 100)
}

object PlayerCharacter {
  val _stats = Lens[PlayerCharacter, CharStats](_.stats)((cs: CharStats) => p => p.copy(stats = cs)(position = p.position))

  val currentHp: PLens[PlayerCharacter, PlayerCharacter, Int, Int] = _stats composeLens CharStats.currentHp
  val str = _stats composeLens CharStats.strength
  val dex = _stats composeLens CharStats.dexterity
  val int = _stats composeLens CharStats.intellect
  val vit = _stats composeLens CharStats.vitality
  val spd = _stats composeLens CharStats.speed
}

trait PlayerChoice {
  bc: BaseCharacter with Magic with UseCards =>

  @tailrec
  final def playerInput(actors: Actors, deckManager: DeckManager): Actors = {
    statement("[A]ttack [M]agic ")

    requirePlayerInput match {
      case "a" => bc.attack(actors.cast, None)
      case "m" => bc.useMagic(actors, deckManager)
      case _ => playerInput(actors, deckManager)
    }
  }
}

trait TopLevelBattleInput extends Choice {
  bc: BaseCharacter with Magic with UseCards =>

  @scala.annotation.tailrec
  final def takeInput(io: UserInput, a: Actors, d: DeckManager): Xor[Choice, (Actors, DeckManager)] = {
    io() match {
      case "a" => Xor.Right(bc.attack(a.cast, None), d)
      case "m" => Xor.Right(bc.useMagic(a, d), d) //FIXME
      case _ => takeInput(io, a, d)
    }
  }
}

trait Choice extends RecursiveChoice[Actors, DeckManager]

trait RecursiveChoice[A, D] {
  def takeInput(io:UserInput, a:A, d: D): Xor[RecursiveChoice[A, D], (A, D)]
}

trait UseCards { bc: BaseCharacter =>

}

case class EnemyCharacter(name: String, stats: CharStats)(override val position: Int = 0)
  extends BaseCharacter {

  override val isPlayer: Boolean = false

  def this(name: String, hp: Int) = {
    this(name, CharStats(hp, hp))()
  }

  override def takeDamage(damage: Int): BaseCharacter = {
    val dmg: (Int) => Int = (toTake: Int) => (- toTake)

    EnemyCharacter.currentHp.modify(dmg)(this)
  }

  def resetPosition = copy()(position = position - 100)
}


object EnemyCharacter {
  def create(name: String, maxHp: Int) = new EnemyCharacter(name, maxHp)

  def slime = EnemyCharacter("Slime", CharStats(10, 10, strength = 4, intellect = 4, speed = 7))()

  def yellowSlime = EnemyCharacter("Yellow Slime", CharStats(5, 15, intellect = 4, vitality = 2))()

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
