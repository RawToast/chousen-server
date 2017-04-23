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


case class Player(name:String, stats: CharStats, position: Int)

case class Cards(cards: List[Card])

case class Card(name: String, description: String)

case class GameMessage(text: String)

case class Dungeon(currentEncounter: Battle, remainingEncounters: Seq[Battle])

case class Battle(enemies: Seq[Enemy])

case class Enemy(name: String, id: UUID, stats: CharStats, position: Int)

object Deck {
  sealed trait Card{
    val name: String
    val description: String
  }

  case object FireballCard extends Card {
    override val name = "Fireball"
    override val description = "Casts a fireball, dealing damage to all enemies"
  }

}

// Enum example
//object WeekDay {
//  sealed abstract class EnumVal(val name: String)
//  case object FireballCard extends EnumVal("Fireball")
//  case object HealCard extends EnumVal("Heal")
//  val daysOfWeek = Seq(FireballCard, HealCard)
//}

