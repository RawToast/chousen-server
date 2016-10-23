package chousen.character

import chousen._
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
  extends BaseCharacter with PlayerChoice with Magic {

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
  bc: BaseCharacter with Magic =>

  @tailrec
  final def playerInput(actors: Actors): Actors = {
    statement("[A]ttack [M]agic")

    requirePlayerInput match {
      case "a" => bc.attack(actors.cast, None)
      case "m" => bc.useMagic(actors)
      case _ => playerInput(actors)
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
