package chousen.data

case class GameResponse(player: Player, cards: Cards, dungeon: Dungeon, messages: Seq[GameMessage])

case class CharStats(maxHp: Int,
                     currentHp: Int,
                     strength: Int = 8,
                     dexterity: Int = 8,
                     intellect: Int = 8,
                     vitality: Int = 8,
                     speed: Int = 8)


object CharStats {

  import monocle.Lens

  val DEFAULT = CharStats(100, 100)
  val DEFAULT_STAT = 8
  val DEFAULT_INTELLECT = DEFAULT_STAT

  private val company = Lens[CharStats, Int](_.currentHp)(c => e => e.copy(currentHp = c))

  val currentHp = Lens[CharStats, Int](_.currentHp)(hp => s => s.copy(currentHp = hp))
  val strength = Lens[CharStats, Int](_.strength)(str => s => s.copy(strength = str))
  val dexterity = Lens[CharStats, Int](_.dexterity)(dex => s => s.copy(dexterity = dex))
  val intellect = Lens[CharStats, Int](_.intellect)(int => s => s.copy(intellect = int))
  val vitality = Lens[CharStats, Int](_.vitality)(vit => s => s.copy(vitality = vit))
  val speed = Lens[CharStats, Int](_.speed)(spd => s => s.copy(speed = spd))
}

case class Player(name:String, stats: CharStats, position: Int)

case class Cards(cards: List[Card])

case class Card(name: String, description: String)

case class GameMessage(text: String)

case class Dungeon(currentEncounter: Battle, remainingEncounters: Seq[Battle])

case class Battle(enemies: Set[Enemy])

case class Enemy(name: String, stats: CharStats)
