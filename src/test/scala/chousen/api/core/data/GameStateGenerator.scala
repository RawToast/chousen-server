package chousen.api.core.data

import java.util.UUID

import chousen.api.data._

import scala.collection.LinearSeq


object GameStateGenerator {

  val uuid = UUID.fromString("624d191b-5561-4c24-ba3f-e646dbb3c13a")

  val playerName = "Test Player"

  val staticGameState = {
    val player = Player(playerName, CharStats(100, 100), 0)
    gameStateWithPlayer(player)
  }

  val gameStateWithFastPlayer = {
    val player = Player(playerName, CharStats(100, 100), 1)
    gameStateWithPlayer(player)
  }

  private def gameStateWithPlayer(player:Player) = {
    import cats.implicits._
    import chousen.api.types.Implicits._

    val cards = Cards(List(Card("Fireball Card", "Casts a fireball, dealing damage to all enemies")))

    def createSlime = Battle(Seq(Enemy("Slime", UUID.randomUUID(), CharStats(10, 10), 0)))

    def createBattle = createSlime |+| createSlime

    val dungeon = Dungeon(createBattle, LinearSeq(createBattle, createBattle |+| createBattle))
    val msgs = Seq.empty[GameMessage]

    GameState(uuid, player, cards, dungeon, msgs)
  }

}
