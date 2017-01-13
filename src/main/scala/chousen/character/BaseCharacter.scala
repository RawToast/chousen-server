package chousen.character

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

  import scala.language.implicitConversions
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
      case "a" => (bc.attack(actors.fullCastWithoutPlayer, None), deckManager)
      case "m" => bc.useMagic(actors, deckManager)
      case _ => playerInput(actors, deckManager)
    }
  }
}

trait TopLevelBattleInput extends Choice {
  bc: BaseCharacter with Magic with UseCards =>

  @scala.annotation.tailrec
  final def takeInput(io: UserInput, a: Cast, d: DeckManager): Either[Choice, (Cast, DeckManager)] = {
    io() match {
      case "a" => Right(bc.attack(a.cast, None) -> d)
      case "m" => Right(bc.useMagic(a, d))
      case "c" => use(a, d)
      case _ => takeInput(io, a, d)
    }
  }
}

trait Choice extends RecursiveChoice[Cast, DeckManager]

trait RecursiveChoice[A, D] {
  def takeInput(io: UserInput, a: A, d: D): Either[RecursiveChoice[A, D], (A, D)]
}

trait UseCards {
  bc: BaseCharacter with TopLevelBattleInput =>

  def use(actors: Cast, dm: DeckManager): Either[Choice, (Cast, DeckManager)] = {

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

          Right(na -> ndm)
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

  implicit val enemyCharacter: (EnemyBuilder) => EnemyCharacter = (eb:EnemyBuilder) => eb.e

  def create(name: String, maxHp: Int=100) = new EnemyCharacter(name, maxHp)

  lazy val slime: EnemyCharacter = EnemyBuilder.make("Slime") hp 10 str 4 int 4 spd 7

  lazy val yellowSlime: EnemyCharacter = EnemyBuilder.make("Yellow Slime") hp 15 vit 2 int 4

  lazy val giantSlime: EnemyCharacter = EnemyBuilder.make("Giant Slime") hp 30 str 18 vit 12 int 4 spd 6

  lazy val baseCharacter: EnemyCharacter = EnemyCharacter.create("Scoundrel")

  lazy val scoundrel = baseCharacter


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

case class EnemyBuilder(e:EnemyCharacter) {

  def hp(x:Int): EnemyBuilder = EnemyBuilder(
    e.copy(stats = e.stats.copy(maxHp = x, currentHp = x))(e.position)
  )

  def str(x:Int): EnemyBuilder = EnemyBuilder(EnemyCharacter.str.set(x)(e))
  def dex(x:Int): EnemyBuilder = EnemyBuilder(EnemyCharacter.dex.set(x)(e))
  def vit(x:Int): EnemyBuilder = EnemyBuilder(EnemyCharacter.vit.set(x)(e))
  def int(x:Int): EnemyBuilder = EnemyBuilder(EnemyCharacter.int.set(x)(e))
  def spd(x:Int): EnemyBuilder = EnemyBuilder(EnemyCharacter.spd.set(x)(e))
}

object EnemyBuilder {
  def make(name:String): EnemyBuilder = EnemyBuilder(EnemyCharacter.create(name))
}
