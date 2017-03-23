package api.types

import api.data.{Battle, Enemy}
import cats.Monoid

trait MonoidInstances {
  implicit val battleMonoid: Monoid[Battle] =
    new Monoid[Battle] {
      override def empty: Battle = Battle(Set.empty[Enemy])

      override def combine(x: Battle, y: Battle): Battle = {
        Battle(x.enemies ++ y.enemies)
      }
    }
}