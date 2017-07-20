package chousen.api.types

import chousen.api.data.{Card, Enemy, Player}

trait Equality[A] {
  def hasSameIdentity(x: A, y: A): Boolean
}

trait EqualitySyntax {
  implicit class EqualityOps[A](value: A) {

    /**
      * Do x and y have the same identity? This returns true if the identifiers for each
      * object match (e.g. they have the same UUID, name, etc) any other values -- their
      * state -- may differ.
      */
    def hasSameIdentity(a:A)(implicit equality: Equality[A]): Boolean = {
      equality.hasSameIdentity(value, a)
    }

    /**
      * @see hasSameIdentity
      */
    def ~= (a:A)(implicit equality: Equality[A]): Boolean = hasSameIdentity(a)(equality)
  }
}

trait EqualityInstances {
  implicit val enemyEquality: Equality[Enemy] =
    (x: Enemy, y: Enemy) => x.id == y.id

  implicit val playerEquality: Equality[Player] =
    (x: Player, y: Player) => x.name == y.name

  implicit val cardEquality: Equality[Card] =
    (x: Card, y: Card) => x.id == y.id
}

object Equality {
  def hasEqualId[A](a: A, b: A)(implicit x: Equality[A]): Boolean = {
    x.hasSameIdentity(a, b)
  }
}

