package chousen.character

import cats.data.Xor
import chousen.cards.DeckManager
import chousen.engine.Engine
import chousen.{Actors, _}

trait Attack extends Action {
  char: BaseCharacter =>

  //TODO: Refactor
  def attack(target: Set[BaseCharacter], bystanders: Option[Set[BaseCharacter]]): Actors = {
    if (target.size == 1) {
      exclaim(s"$char attacks")

      complete(target, bystanders)
    } else {
      statement("Select a target:")

      val targets: Map[String, BaseCharacter] = target.foldLeft(Map.empty[Int, BaseCharacter]) {
        (m, bc: BaseCharacter) =>
          if (m.isEmpty) m + ((1, bc))
          else m.+((m.keySet.max + 1, bc))
      }.foldLeft(Map.empty[String, BaseCharacter])((m, bc) => m.+((bc._1.toString, bc._2)))

      val targetString = targets.map(kv => s"[${kv._1}]:${kv._2} ").mkString
      statement(targetString)

      targets.get(requirePlayerInput)
        .map(bc =>
          complete(Set(bc),
            Option(bystanders.getOrElse(Set.empty[BaseCharacter]) ++ targets.values.toSet - bc))
        ).getOrElse(attack(target, bystanders))
    }
  }

  override def complete(target: Set[BaseCharacter], bystanders: Option[Set[BaseCharacter]]): Actors = {
    val t = target.map { e =>
      val damage = Engine.calcDamage(char, e)
      if (isPlayer) exclaim(s"$char deals $damage to $e")
      e.takeDamage(damage)
    }
    Actors(char, t ++ bystanders.getOrElse(Set.empty))
  }
}


