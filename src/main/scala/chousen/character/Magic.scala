package chousen.character

import chousen.engine.Engine
import chousen.{Actors, _}

trait Magic extends Action { char:BaseCharacter with PlayerChoice =>
  val spellBook: SpellBook

  def useMagic(actors: Actors):Actors = {
    if (spellBook.availableSpells.isEmpty) {statement(s"$char does not know any more magic"); playerInput(actors)}
    else {
      def selectSpell: Spell = {
        statement(s"Select spell: ${spellBook.spellList}")
        spellBook.spellMap.getOrElse(requirePlayerInput, selectSpell)
      }

      selectSpell.cast(char, actors.cast)
    }
  }
}

case class SpellBook(spells:Set[Spell]) {

  lazy val availableSpells = spells.toList.sortBy(_.name)

  val spellMap: Map[String, Spell] =
      availableSpells.foldLeft(Map.empty[String, Spell])((m, s) =>
        m + (((if (m.isEmpty) "a" else m.keySet.max.head + 1).toString, s)))

  val spellList = spellMap.map(kv => s"[${kv._1}]:${kv._2} ").mkString

  def withSpell(spell:Spell) = this.copy(spells + spell)
}

object SpellBook {
  def create = SpellBook(Set.empty)
}

trait Spell {
  val name: String
  val description: String
  val magicType: String

  val baseDamage: Int
  val maxCopies: Int = 4

  def cast(user: BaseCharacter, target: Set[BaseCharacter], bystanders: Option[Set[BaseCharacter]]=None): Actors

  def complete(user: BaseCharacter, target: Set[BaseCharacter], bystanders: Option[Set[BaseCharacter]]): Actors
}

trait AoeSpell extends Spell {
  override def cast(user:BaseCharacter, target: Set[BaseCharacter], bystanders: Option[Set[BaseCharacter]]=None): Actors = {
    target.map(bc => bc.takeDamage(Engine.calcMagic(this, user, bc)))
    complete(user, target, bystanders)
  }
}

class FireBall extends AoeSpell {

  val name = "Fireball"

  val description: String = "Deals weak fire damage to all enemies"

  val magicType: String = "fire"

  val baseDamage: Int = 3

  def complete(user: BaseCharacter, target: Set[BaseCharacter], bystanders: Option[Set[BaseCharacter]]): Actors = {
    val t = target.map { e:BaseCharacter =>
      val damage = Engine.calcMagic(this, user, e)
      exclaim(s"$user deals $damage $magicType damage to $e")
      e.takeDamage(damage)
    }
    Actors(user, t ++ bystanders.getOrElse(Set.empty))
  }
}

