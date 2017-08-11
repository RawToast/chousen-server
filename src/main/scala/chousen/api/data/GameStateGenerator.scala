package chousen.api.data

import java.util.UUID

import chousen.game.cards.{CardCatalogue, CardManager}

import scala.collection.LinearSeq


object GameStateGenerator {

  val uuid: UUID = UUID.fromString("624d191b-5561-4c24-ba3f-e646dbb3c13a")

  val playerName = "Test Player"

  val firstEnemy: Enemy = createBigSlime(UUID.fromString("299dcde7-88e0-4e1d-9024-69baa0fda0a2"))
  val secondEnemy: Enemy = createBigSlime(UUID.fromString("403768ae-a336-4654-bebf-6920ff4d5eb8"))

  val staticGameState: GameState = {
    val player = Player(playerName, "Static", CharStats(100, 100), Experience(), Equipment(None, None), 0)
    gameStateWithPlayer(player)
  }

  val gameStateWithFastPlayer: GameState = {
    val player = Player(playerName, "FastStatic", CharStats(100, 100), Experience(), Equipment(None, None), 1)
    gameStateWithPlayer(player)
  }

  lazy val crushingBlowCard = Card(UUID.fromString("614e566c-03a5-43b0-ae55-e131f4428fc3"), "Crushing Blow", "Deals heavy damage to a single target", CrushingBlow)

  private def gameStateWithPlayer(player:Player) = {
    import cats.implicits._
    import chousen.Implicits._

    val cards = CardManager.startGame(CardCatalogue.defaultDeck, CardCatalogue.passiveCards)
    def mkBattle(e: Enemy) = Battle(Set(e))
    def createBattle = mkBattle(firstEnemy) |+| mkBattle(secondEnemy)

    val dungeon = Dungeon(createBattle, LinearSeq(createBattle, createBattle |+| createBattle))
    val msgs = Seq.empty[GameMessage]

    GameState(uuid, player, cards, dungeon, msgs)
  }

  private def createBigSlime(id: UUID) = Enemy("Slime", id, CharStats(999, 999), 0)

}
