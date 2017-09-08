package chousen.game.core

import java.util.UUID

import chousen.api.data.PlayerOptics.{PlayerClassLens, SetPlayerStats}
import chousen.api.data._
import chousen.game.cards.{CardManager, CardCatalogue => CC}
import chousen.game.core.GameStateOptics.EncounterLens
import chousen.game.dungeon.DungeonBuilder

import scala.util.Random

class RandomGameStateCreator(dungeonBuilder: DungeonBuilder) extends GameStateCreation {

  def create(name: String, choice:Int=0, uuid: UUID = UUID.randomUUID()): GameState = {
    //val seed = new scala.util.Random().nextInt(2)
    val dungeonSeed = new scala.util.Random().nextInt(6)
    val dungeonSeed2 = new scala.util.Random().nextInt(6)
    val dungeonSeed3 = new scala.util.Random().nextInt(6)

    val p = Player(name, "Rouge",
      CharStats(70, 70, strength = 6, dexterity = 6, intellect = 6, vitality = 6),
      Experience(), Equipment(None, None), 10, 0)

    val dungeon = dungeonBuilder.makeDungeon(dungeonSeed, dungeonSeed2, dungeonSeed3)

    val player = choice match {
      case 1 => SetPlayerStats.apply(2, 1, -1, 1).compose(PlayerClassLens.set("Fighter"))(p)
      case 2 => SetPlayerStats.apply(2, 0, -1, 2).compose(PlayerClassLens.set("Berserker"))(p)
      case 3 => SetPlayerStats.apply(1, 0, 0, 2).compose(PlayerClassLens.set("Chieftain"))(p)
      case 4 => SetPlayerStats.apply(1, 1, 0, 1).compose(PlayerClassLens.set("Rogue"))(p)
      case 5 => SetPlayerStats.apply(0, 1, 1, 1).compose(PlayerClassLens.set("Trickster"))(p)
      case 6 => SetPlayerStats.apply(0, 0, 2, 1).compose(PlayerClassLens.set("Mage"))(p)
      case _ => SetPlayerStats.apply(0, 0, 3, 0).compose(PlayerClassLens.set("Wizard"))(p)
    }

    val deck = choice match {
      case 1 => CC.fighterDeck
      case 2 => CC.berserkerDeck
      case 3 => CC.chieftainDeck
      case 4 => CC.rogueDeck
      case 5 => CC.tricksterDeck
      case 6 => CC.mage
      case _ => CC.wizard
    }

    val cards: Cards = CardManager.startGame(deck, CC.passiveCards)


    val dungeonTreasure: Seq[Card] = Random.shuffle(Seq (
      CC.rarePepe, CC.rarePepe, CC.rarePepe, CC.rarePepe,
      CC.elixirOfStrength, CC.elixirOfDexterity, CC.elixirOfVitality, CC.elixirOfIntelligence,

      CC.potionOfMiasma, CC.potionOfMiasma,
      // UNIQUES :D

      CC.troggsAnnilator, CC.manamune, CC.wandOfDefiance, CC.deceiver, CC.magePlate
    ))

    val msgs = Seq.empty[GameMessage]

    GameState(uuid, player, cards.copy(treasure = dungeonTreasure), dungeon, msgs)
  }

}

trait GameStateCreation {

  def create(name: String, choice: Int=0, uuid: UUID = UUID.randomUUID()): GameState

  def createAndStart(name: String, choice: Int=0, uuid: UUID = UUID.randomUUID()): GameState = {
    start(create(name, choice, uuid))
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