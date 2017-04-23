package game
//
// import chousen.cards.{Deck, DeckManager}
// import chousen.character.BaseCharacter//, EnemyCharacter, PlayerCharacter}
// import chousen.core.Encounter
// import api.data.CharStats
// import chousen.engine.State
//
// import scala.annotation.tailrec
//
// object Main extends App {
//
//   statement("Enter name: ")
//   val name = requireCaseSensitivePlayerInput
//   val initDeck = Deck.create
//
//   val player = PlayerCharacter(name, CharStats.DEFAULT)()
//
//   val defaultDeck = DeckManager.startNewGameWithDefaultDeck
//
//   val firstEncounter = Encounter.create(EnemyCharacter.yellowSlime) + EnemyCharacter.slime
//   val secondEncounter = firstEncounter + EnemyCharacter.giantSlime
//   val thirdEncounter = Encounter.create(EnemyCharacter.scoundrel)
//
//   val dungeon = core.Dungeon(List(firstEncounter, secondEncounter, thirdEncounter))
//
//   GameLoop(name).loop(player, defaultDeck, dungeon)
// }
//
// case class GameLoop(playerName: String) {
//   story(s"$playerName has entered the dungeon")
//   story(s"It was dark and smelly")
//   statement(s"Eventually $playerName finds a room with a chest!")
//
//   def loop(p: PlayerCharacter, deckManager: DeckManager, dungeon: core.Dungeon) = {
//
//     break()
//     @tailrec
//     def innerLoop(actors: Cast, dm: DeckManager): State = {
//
//       // Move
//       val (newCast:Cast, nxtDm: DeckManager) = actors.takeTurn(dm)
//
//       val state = newCast.postAttackState
//
//       if (!state.playerAlive || !state.actors.hasEnemies) state
//       else innerLoop(state.actors.changeTurn, nxtDm)
//     }
//
//     @tailrec
//     def play(player: PlayerCharacter, d: core.Dungeon): core.Dungeon = {
//       val encounterOption: Option[Encounter] = d.nextEncounter
//       val encounter = encounterOption.get
//
//       exclaim(encounter.toString)
//
//       val cast = Peoples.init(player, encounter.enemies)
//
//       // Fight
//       val result: State = innerLoop(cast, deckManager)
//
//       // Conclude
//       val newDungeon = d.progress
//
//       if (result.playerAlive && newDungeon.isNotComplete) play(result.actors.player, newDungeon)
//       else d
//     }
//
//     if (play(p, dungeon).isNotComplete) {
//       exclaim("Game over")
//     } else {
//       story(s"$playerName found a wooden chest")
//       story(s"$playerName opens the wooden chest")
//       break()
//       suspense(s"Unfortunately")
//       story(s" it was a trap and $playerName died")
//     }
//   }
// }
//
//