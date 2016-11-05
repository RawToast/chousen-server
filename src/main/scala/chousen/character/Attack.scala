package chousen.character

import chousen._
import chousen.cards.DeckManager
import chousen.engine.Engine

trait Attack extends Action {
  char: BaseCharacter =>

  //TODO: Refactor
  def attack(target: Set[BaseCharacter], bystanders: Option[Set[BaseCharacter]]): Cast = {
    if (target.size == 1) {
      exclaim(s"$char attacks")

      complete(target, bystanders)
    } else {
      statement("Select a target:")

      object TargetUtil extends Options[BaseCharacter] {
        override val items = target.toList
      }

      lazy val targets: Map[String, BaseCharacter] = TargetUtil.options

      statement(TargetUtil.optionString)

      targets.get(requirePlayerInput)
        .map(bc =>
          complete(Set(bc),
            Option(bystanders.getOrElse(Set.empty[BaseCharacter]) ++ targets.values.toSet - bc))
        ).getOrElse(attack(target, bystanders))
    }
  }

  override def complete(target: Set[BaseCharacter], bystanders: Option[Set[BaseCharacter]]): Cast = {
    val t = target.map { e =>
      val damage = Engine.calcDamage(char, e)
      if (isPlayer) exclaim(s"$char deals $damage to $e")
      e.takeDamage(damage)
    }
    Actors(char, t ++ bystanders.getOrElse(Set.empty))
  }
}


