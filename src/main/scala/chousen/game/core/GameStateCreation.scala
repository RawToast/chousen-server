package chousen.game.core

import java.util.UUID

import chousen.api.data.PlayerOptics.{PlayerClassLens, SetPlayerStats}
import chousen.api.data._
import chousen.game.cards.{CardCatalogue, CardManager}
import chousen.game.core.GameStateOptics.EncounterLens
import chousen.game.dungeon.DungeonBuilder

class RandomGameStateCreator(dungeonBuilder: DungeonBuilder) extends GameStateCreation {

  def create(name: String, uuid: UUID = UUID.randomUUID()): GameState = {
    val seed = new scala.util.Random().nextInt(6)
    val dungeonSeed = new scala.util.Random().nextInt(6)
    val dungeonSeed2 = new scala.util.Random().nextInt(6)
    val dungeonSeed3 = new scala.util.Random().nextInt(6)

    val p = Player(name, "Rouge", CharStats(100, 100), Experience(), 0)

    val dungeon = dungeonBuilder.makeDungeon(dungeonSeed, dungeonSeed2, dungeonSeed3)

    val player = seed match {
      case 0 => SetPlayerStats.apply(2, 0, 0, 1).compose(PlayerClassLens.set("Warrior"))(p)
      case 1 => SetPlayerStats.apply(0, 3, 0, 0).compose(PlayerClassLens.set("Assassin"))(p)
      case 2 => SetPlayerStats.apply(-1, 0, 4, 0).compose(PlayerClassLens.set("Wizard"))(p)
      case 3 => SetPlayerStats.apply(0, 1, 1, 1).compose(PlayerClassLens.set("Jester"))(p)
      case 4 => SetPlayerStats.apply(1, 1, 0, 1).compose(PlayerClassLens.set("Barbarian"))(p)
      case _ => SetPlayerStats.apply(0, 0, 0, 2).compose(PlayerClassLens.set("Rogue"))(p)
    }

    val cards: Cards = seed match {
      case 0 => CardManager.startGame(CardCatalogue.strengthDeck)
      case 1 => CardManager.startGame(CardCatalogue.dexterityDeck)
      case 2 => CardManager.startGame(CardCatalogue.magicDeck)
      case 3 => CardManager.startGame(CardCatalogue.cheeseDeck)
      case 4 => CardManager.startGame(CardCatalogue.strongManDeck)
      case _ => CardManager.startGame(CardCatalogue.defaultDeck)
    }

    val msgs = Seq.empty[GameMessage]

    GameState(uuid, player, cards, dungeon, msgs)
  }

}

trait GameStateCreation {

  def create(name: String, uuid: UUID = UUID.randomUUID()): GameState

  def createAndStart(name: String, uuid: UUID = UUID.randomUUID()): GameState = {
    start(create(name, uuid))
  }

  def start(game: GameState): GameState = {
    val update = EncounterLens.modify {
      case (p: Player, es: Set[Enemy], m: Seq[GameMessage]) =>

        val msgs = Seq(GameMessage(s"${p.name} has entered the dungeon"),
          startEncounterMessage(es, p))

        GameOps.updateUntilPlayerIsActive(p, es, m ++ msgs)
    }

    update(game)
  }

  def startEncounterMessage(enemies: Set[Enemy], player: Player): GameMessage = {
    if (enemies.size == 1) GameMessage(s"${player.name} is attacked by ${enemies.head.name}!")
    else GameMessage(s"${player.name} is attacked by: ${enemies.toList.map(_.name).mkString(", ")}!")
  }
}