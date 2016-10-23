package chousen.character

import chousen.engine.{Dice, Engine}
import chousen.{Actors, _}

trait Magic extends Action {
  char: BaseCharacter with PlayerChoice =>
  val spellBook: SpellBook

  def useMagic(actors: Actors): Actors = {
    if (spellBook.availableSpells.isEmpty) {
      statement(s"$char does not know any more magic"); playerInput(actors)
    }
    else {
      def selectSpell: Spell = {
        statement(s"Select spell: ${spellBook.spellList}")
        spellBook.spellMap.getOrElse(requirePlayerInput, selectSpell)
      }

      selectSpell.complete(char, actors.cast)
    }
  }
}

case class SpellBook(spells: Set[Spell]) {

  lazy val availableSpells = spells.toList.sortBy(_.name)

  val spellMap: Map[String, Spell] =
    availableSpells.foldLeft(Map.empty[String, Spell])((m, s) =>
      m + (((if (m.isEmpty) "a" else m.keySet.max.head + 1).toString, s)))

  val spellList = spellMap.map(kv => s"[${kv._1}]:${kv._2} ").mkString

  def withSpell(spell: Spell) = this.copy(spells + spell)
}

object SpellBook {
  def create = SpellBook(Set.empty)
}


trait CardAction {
  val name: String
  val description: String
  val maxCopies: Int = 4

  def complete(user: BaseCharacter, target: Set[BaseCharacter], bystanders: Option[Set[BaseCharacter]]): Actors
}


trait Spell extends CardAction {

  val magicType: String
  val baseDamage: Int

  def complete(user: BaseCharacter, target: Set[BaseCharacter], bystanders: Option[Set[BaseCharacter]] = None): Actors
}

class FireBall extends Spell {

  val name = "Fireball"

  val description: String = "Deals weak fire damage to all enemies"

  val magicType: String = "fire"

  val baseDamage: Int = 3

  def complete(user: BaseCharacter, target: Set[BaseCharacter], bystanders: Option[Set[BaseCharacter]] = None): Actors = {
    val t = target.map { e: BaseCharacter =>
      val damage = Engine.calcMagic(this, user, e)
      exclaim(s"$user deals $damage $magicType damage to $e")
      e.takeDamage(damage)
    }
    Actors(user, t ++ bystanders.getOrElse(Set.empty))
  }
}

trait Potion extends CardAction {

  val drink: BaseCharacter => BaseCharacter

  def complete(user: BaseCharacter, target: Set[BaseCharacter], bystanders: Option[Set[BaseCharacter]] = None): Actors
}

class HealWounds extends Potion {

  val name = "Heal Wounds"

  val description: String = "Considerably Heals the user"

  val drink: (BaseCharacter) => BaseCharacter =
    bc => {
      val variance = Dice.roll() + Dice.roll()

      val hp: Int = bc.stats.maxHp.min(bc.stats.currentHp + bc.stats.vitality + (bc.stats.maxHp / 10) + variance)
      val diff = hp - bc.stats.currentHp

      exclaim(s"$bc heals ${diff}HP to $hp")

      bc match {
        case poc: PlayerCharacter => PlayerCharacter.currentHp.set(hp)(poc)
        case emy: EnemyCharacter => EnemyCharacter.currentHp.set(hp)(emy)
      }
    }

  def complete(user: BaseCharacter, target: Set[BaseCharacter], bystanders: Option[Set[BaseCharacter]] = None): Actors = {
    Actors(drink(user), target ++ bystanders.getOrElse(Set.empty))
  }
}

