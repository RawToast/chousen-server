package chousen

import api.data.{Enemy, GameMessage, Player}

package object core {
  type Actors = (Player, Set[Enemy])
  type EncounterData = (Player, Set[Enemy], Seq[GameMessage])
  type EncounterUpdate = ((Player, Set[Enemy], Seq[GameMessage])) => (Player, Set[Enemy], Seq[GameMessage])


//  trait ActiveIdentifier[A, B, C] {
//    def getActive(x: A): Either[B, C]
//  }
//
//
//  implicit class ActiveIdentifierSyntax[A, B, C](value: A)(implicit eda: ActiveIdentifier[A, B, C]) {
//    def active: Either[B, C] = {
//      eda.getActive(value)
//    }
//  }
//
//
//  implicit val _encounterData: ActiveIdentifier[(Player, Set[Enemy], Seq[GameMessage]), Player, Enemy] {
//    def getActive(x: (Player, Set[Enemy], Seq[GameMessage])): Either[Player, Enemy]
//  } = new ActiveIdentifier[EncounterData, Player, Enemy] {
//
//    override def getActive(x: EncounterData): Either[api.data.Player, api.data.Enemy] = {
//      val (p: Player, es: Set[Enemy], _) = x
//      val maxPosition = math.max(p.position, es.maxBy(_.position).position)
//
//      if (p.position == maxPosition) Left(p)
//      else Right(es.maxBy(_.position))
//
//    }
//  }
}
