package chousen.api.data

import java.util.UUID

case class GameState(uuid: UUID, player: Player, cards: Cards, dungeon: Dungeon, messages: Seq[GameMessage])

case class CharStats(maxHp: Int,
                     currentHp: Int,
                     strength: Int = 8,
                     dexterity: Int = 8,
                     intellect: Int = 8,
                     vitality: Int = 8,
                     speed: Int = 8)


case class Player(name:String, className: String, stats: CharStats, experience: Experience, position: Int)

case class Experience(current: Int=0, next: Int=3, level: Int=1)

case class Cards(hand: Seq[Card], deck: Seq[Card], discard: Seq[Card])

case class Card(id: UUID, name: String, description: String, action: Action)

case class GameMessage(text: String)

case class Dungeon(currentEncounter: Battle, remainingEncounters: Seq[Battle])

case class Battle(enemies: Set[Enemy])

sealed trait Encounterable

case class Enemy(name: String, id: UUID, stats: CharStats, position: Int) extends Encounterable

object Deck {
  sealed trait Card {
    val name: String
    val description: String
  }
}