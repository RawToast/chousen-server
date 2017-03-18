package api.data

import chousen.cards.DeckManager
import chousen.character.PlayerCharacter
import chousen.core.Encounter

object Implicits extends LegacyConversions

object ImplicitSyntax

trait LegacyConversions{
  implicit val fromPlayerCharacter: (PlayerCharacter) => Player =
    pc => Player(pc.name, pc.stats, pc.position)

  implicit val cardConversion: DeckManager => Cards = dm =>
    Cards(dm.hand.items.map(c => Card(c.active.name, c.active.description)))

  implicit val encounterConv: Encounter => Battle = enc =>
    Battle(enc.enemies.map(e => Enemy(e.name, e.id, e.stats, e.position)))

  implicit val dungeonConv: chousen.core.Dungeon => Dungeon = dng =>
    Dungeon(dng.current, dng.encounters.tail.map(encounterConv))
}
