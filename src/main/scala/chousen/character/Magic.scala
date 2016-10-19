package chousen.character

import chousen.engine.Engine
import chousen.{Actors, _}

trait Magic { char:BaseCharacter =>
  val spellBook: SpellBook
}

case class SpellBook(spells:Set[Spell]) {

  val availableSpells = spells.filter(_.copies > 0).toList.sortBy(_.name)

  val spellMap: Map[String, Spell] = availableSpells.
    foldLeft(Map.empty[String, Spell])((m, s) => m + (((m.keySet.max.head + 1).toString, s)))

  val spellList = spellMap.map(kv => s"[${kv._1}]:${kv._2} ").mkString
}

trait Spell extends Action {
  char: BaseCharacter =>

  val name: String
  val description: String
  val magicType: String

  val baseDamage: Int
  val copies: Int = 4
  val maxCopies: Int = 4

  def useMagic(target: Set[BaseCharacter], bystanders: Option[Set[BaseCharacter]]): Actors

  def complete(target: Set[BaseCharacter], bystanders: Option[Set[BaseCharacter]]): Actors
}

trait FireBall extends Spell {
  char: BaseCharacter =>

  val name = "Fireball"

  val description: String = "Deals weak fire damage to all enemies"

  val magicType: String = "fire"

  val baseDamage: Int = 5

  def attack(target: Set[BaseCharacter], bystanders: Option[Set[BaseCharacter]]): Actors = {
    target.map(bc => bc.takeDamage(Engine.calcMagic(this, char, bc)))
    complete(target, bystanders)
  }

  override def complete(target: Set[BaseCharacter], bystanders: Option[Set[BaseCharacter]]): Actors = {
    val t = target.map { e:BaseCharacter =>
      val damage = Engine.calcMagic(this, char, e)
      if (isPlayer) exclaim(s"$char deals $damage $magicType damage to $e")
      e.takeDamage(damage)
    }
    Actors(char, t ++ bystanders.getOrElse(Set.empty))
  }
}

