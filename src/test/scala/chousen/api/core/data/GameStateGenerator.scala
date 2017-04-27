package chousen.api.core.data

import java.util.UUID

import chousen.api.data._

import scala.collection.LinearSeq


object GameStateGenerator {

  val uuid: UUID = UUID.fromString("624d191b-5561-4c24-ba3f-e646dbb3c13a")

  val playerName = "Test Player"

  val firstEnemy: Enemy = createSlime(UUID.fromString("299dcde7-88e0-4e1d-9024-69baa0fda0a2"))
  val secondEnemy: Enemy = createSlime(UUID.fromString("403768ae-a336-4654-bebf-6920ff4d5eb8"))

  val staticGameState: GameState = {
    val player = Player(playerName, CharStats(100, 100), 0)
    gameStateWithPlayer(player)
  }

  val gameStateWithFastPlayer: GameState = {
    val player = Player(playerName, CharStats(100, 100), 1)
    gameStateWithPlayer(player)
  }

  private def gameStateWithPlayer(player:Player) = {
    import cats.implicits._
    import chousen.api.types.Implicits._

    val cards = Cards(List(Card("Fireball Card", "Casts a fireball, dealing damage to all enemies")))
    def mkBattle(e: Enemy) = Battle(Set(e))
    def createBattle = mkBattle(firstEnemy) |+| mkBattle(secondEnemy)

    val dungeon = Dungeon(createBattle, LinearSeq(createBattle, createBattle |+| createBattle))
    val msgs = Seq.empty[GameMessage]

    GameState(uuid, player, cards, dungeon, msgs)
  }

  private def createSlime(id:UUID) = Enemy("Slime", id, CharStats(10, 10), 0)


}
