package chousen.api.data

import java.util.UUID

case class GameState(id: UUID, player: Player, cards: Cards, dungeon: Dungeon, messages: Seq[GameMessage])

case class CharStats(maxHp: Int,
                     currentHp: Int,
                     strength: Int = 8,
                     dexterity: Int = 8,
                     intellect: Int = 8,
                     vitality: Int = 8,
                     speed: Int = 8)


case class Player(name:String, stats: CharStats, position: Int, status: Seq[Status] = Seq.empty)

case class Cards(hand: Seq[Card], deck: Seq[Card], discard: Seq[Card])

case class Card(id: UUID, name: String, description: String, action: Action)

case class GameMessage(text: String)

case class Dungeon(currentEncounter: Battle, remainingEncounters: Seq[Battle])

case class Battle(enemies: Set[Enemy])

case class Enemy(name: String, id: UUID, stats: CharStats, position: Int)

object Deck {
  sealed trait Card {
    val name: String
    val description: String
  }
}


case class Status(effect: StatusEffect, description: String, amount: Int, turns: Int)

sealed trait StatusEffect

case object Fast extends StatusEffect
case object StoneSkin extends StatusEffect
case object Might extends StatusEffect
case object Dexterity extends StatusEffect
case object Smart extends StatusEffect
