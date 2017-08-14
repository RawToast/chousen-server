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


case class Player(name:String, className: String, stats: CharStats, experience: Experience, equipment: Equipment, position: Int, status: Seq[Status] = Seq.empty)

case class Experience(current: Int=0, next: Int=3, level: Int=1)

case class Cards(hand: Seq[Card], deck: Seq[Card], discard: Seq[Card], passive: Seq[Card], equippedCards: EquippedCards)

case class EquippedCards(weapon: Option[Card]=None, armour: Option[Card]=None)

case class Equipment(weapon: Option[Weapon]=None, armour: Option[Armour]=None)

case class Weapon(cardId: UUID, name: String, dmg: Int, requirements: Requirements=Requirements(), effects: Seq[WeaponEffect]=List.empty)

case class Armour(cardId: UUID, name: String, defense: Int, requirements: Requirements=Requirements())

case class Requirements(str: Option[Int]=None, dex: Option[Int]=None, int: Option[Int]=None)

case class Card(id: UUID, name: String, description: String, action: Action, charges: Option[Int]=None)

case class GameMessage(text: String)

case class Dungeon(currentEncounter: Battle, remainingEncounters: Seq[Battle])

case class Battle(enemies: Set[Enemy])

sealed trait Encounterable

case class Enemy(name: String, id: UUID, stats: CharStats, position: Int, status: Seq[Status]=Seq.empty) extends Encounterable

case class Status(effect: StatusEffect, description: String, turns: Int, amount: Option[Int] = None)

sealed trait StatusEffect

case object Fast extends StatusEffect
case object Slow extends StatusEffect

case object StoneSkin extends StatusEffect
case object Might extends StatusEffect
case object Block extends StatusEffect
case object Dexterity extends StatusEffect
case object Smart extends StatusEffect
case object Rage extends StatusEffect
case object Poison extends StatusEffect

sealed trait WeaponEffect

case object Magic extends WeaponEffect
case object Crush extends WeaponEffect
case object Toxic extends WeaponEffect
