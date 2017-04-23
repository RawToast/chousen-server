package chousen.api.types

import cats.Monoid
import chousen.api.data.{Battle, Enemy}

trait MonoidInstances {
  implicit val battleMonoid: Monoid[Battle] =
    new Monoid[Battle] {
      override def empty: Battle = Battle(Seq.empty[Enemy])

      override def combine(x: Battle, y: Battle): Battle = {
        Battle(x.enemies ++ y.enemies)
      }
    }
}