package chousen.game.core

import java.util.UUID

import chousen.api.data.PlayerOptics.{PlayerClassLens, SetPlayerStats}
import chousen.api.data._
import chousen.game.cards.{CardCatalogue, CardManager}
import chousen.game.core.GameStateOptics.EncounterLens
import chousen.game.dungeon.DungeonBuilder

class RandomGameStateCreator(dungeonBuilder: DungeonBuilder) extends GameStateCreation {

  def create(name: String, uuid: UUID = UUID.randomUUID()): GameState = {
    val seed = new scala.util.Random().nextInt(2)
    val dungeonSeed = new scala.util.Random().nextInt(6)
    val dungeonSeed2 = new scala.util.Random().nextInt(6)
    val dungeonSeed3 = new scala.util.Random().nextInt(6)

    val p = Player(name, "Rouge", CharStats(100, 100), Experience(), 0)

    val dungeon = dungeonBuilder.makeDungeon(dungeonSeed, dungeonSeed2, dungeonSeed3)

    val player = seed match {
      case 0 => SetPlayerStats.apply(2, 0, 0, 1).compose(PlayerClassLens.set("Barbarian"))(p)
      case 1 => SetPlayerStats.apply(1, 1, 0, 1).compose(PlayerClassLens.set("Gladiator"))(p)
      case _ => SetPlayerStats.apply(0, 0, 0, 2).compose(PlayerClassLens.set("Warrior"))(p)
    }

    val cards: Cards = CardManager.startGame(CardCatalogue.defaultDeck, CardCatalogue.passiveCards)

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