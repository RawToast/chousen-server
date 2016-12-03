package chousen.character

import chousen.cards.DeckManager
import chousen.engine.{Dice, Engine}
import chousen.{Cast, _}

trait Magic extends Action {
  char: BaseCharacter with PlayerChoice =>
  val spellBook: SpellBook

  def useMagic(cast: Cast, dm: DeckManager): (Cast, DeckManager) = {
    if (spellBook.availableSpells.isEmpty) {
      statement(s"$char does not know any more magic"); playerInput(cast, dm)
    }
    else {
      def selectSpell: Spell = {
        statement(s"Select spell: ${spellBook.spellList}")
        spellBook.spellMap.getOrElse(requirePlayerInput, selectSpell)
      }

      (selectSpell.complete(char, cast.cast), dm)// TODO: Unused DeckManager
    }
  }
}

case class SpellBook(spells: Set[Spell]) {

  lazy val availableSpells = spells.toList.sortBy(_.name)

  lazy val spellMap: Map[String, Spell] =
    availableSpells.foldLeft(Map.empty[String, Spell])((m, s) =>
      m + (((if (m.isEmpty) "a" else m.keySet.max.head + 1).toString, s)))

  lazy val spellList = spellMap.map(kv => s"[${kv._1}]:${kv._2} ").mkString

  def withSpell(spell: Spell) = this.copy(spells + spell)
}

object SpellBook {
  def create = SpellBook(Set.empty)
}


trait CardAction {
  val name: String
  val description: String
  val maxCopies: Int = 4

  def complete(user: BaseCharacter, target: Set[BaseCharacter], bystanders: Option[Set[BaseCharacter]]): Cast
}


trait Spell extends CardAction {

  val magicType: String
  val baseDamage: Int

  def complete(user: BaseCharacter, target: Set[BaseCharacter], bystanders: Option[Set[BaseCharacter]] = None): Cast
}

class FireBall extends Spell {

  val name = "Fireball"

  val description: String = "Deals weak fire damage to all enemies"

  val magicType: String = "fire"

  val baseDamage: Int = 3

  def complete(user: BaseCharacter, target: Set[BaseCharacter], bystanders: Option[Set[BaseCharacter]] = None) = {
    val t = target.map { e: BaseCharacter =>
      val damage = Engine.calcMagic(this, user, e)
      exclaim(s"$user deals $damage $magicType damage to $e")
      e.takeDamage(damage)
    }

    val all = t + user ++ bystanders.getOrElse(Set.empty)

    val player = all.find(p => p.isPlayer)

    Peoples(player.get.asInstanceOf[PlayerCharacter], all.filter(p => p.isPlayer))

    // Actors(user, t ++ bystanders.getOrElse(Set.empty))
  }
}

trait Potion extends CardAction {

  val drink: BaseCharacter => BaseCharacter

  def complete(user: BaseCharacter, target: Set[BaseCharacter], bystanders: Option[Set[BaseCharacter]] = None): Cast
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

  def complete(user: BaseCharacter, target: Set[BaseCharacter], bystanders: Option[Set[BaseCharacter]] = None): Cast = {
   //  Actors(drink(user), target ++ bystanders.getOrElse(Set.empty))

    val all = target + drink(user) ++ bystanders.getOrElse(Set.empty)

    val player = all.find(p => p.isPlayer)

    Peoples(player.get.asInstanceOf[PlayerCharacter], all.filter(p => p.isPlayer))
  }
}

