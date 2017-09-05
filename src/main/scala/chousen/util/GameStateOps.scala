package chousen.util

import java.util.UUID

import chousen.api.data._


trait GameStateOps {

  def toGameResponse(gs: GameState): GameResponse = {


    def mkChargesStr(c: Card) = for {
      charges <- c.charges
      max <- c.maxCharges
    } yield s"($charges/$max)"

    def canPlayCard(c :Card):Boolean = if (c.name.contains("Essence")) {
      if (gs.cards.playedEssence) false else true
    } else true

    def toCardResponse(c: Card):CardResponse = c.action match {
      case _: SingleTargetAction => CardResponse(c.name, c.description, c.id, mkChargesStr(c), canPlayCard(c),
        ActionRequest(c.name, c.description, s"game/${gs.uuid}/single/${c.id}",
          gs.dungeon.currentEncounter.enemies.toSeq.map(e =>
            ActionRequestBody(e.name, Some(c.action), targetId = Option(e.id)))))

      case _: MultiAction => CardResponse(c.name, c.description, c.id, mkChargesStr(c), canPlayCard(c),
        ActionRequest(c.name, c.description, s"game/${gs.uuid}/multi/${c.id}",
          Seq(ActionRequestBody(c.name, Some(c.action), targetIds = Option(gs.dungeon.currentEncounter.enemies.map(_.id))))))

      case _: SelfAction => CardResponse(c.name, c.description,  c.id, mkChargesStr(c), canPlayCard(c),
        ActionRequest(c.name, c.description, s"game/${gs.uuid}/self/${c.id}",
          Seq(ActionRequestBody(c.name, Some(c.action)))))

      case _: CardAction => CardResponse(c.name, c.description,  c.id, mkChargesStr(c), canPlayCard(c),
        ActionRequest(c.name, c.description, s"game/${gs.uuid}/card/${c.id}",
          c.action match {
            case action: DiscardCardAction =>
              action match {
                case ReduceRequirements => gs.cards.hand
                  .filter(r => r.requirements.str.nonEmpty && r.requirements.dex.nonEmpty && r.requirements.int.nonEmpty)
                  .map(h => ActionRequestBody(h.name, Some(c.action), cardId = Option(h.id)))
                case IncreaseCharges => gs.cards.hand
                  .filter(_.charges.nonEmpty).map(h => ActionRequestBody(h.name, Some(c.action), cardId = Option(h.id)))
                case _ => gs.cards.hand.map(h => ActionRequestBody(h.name, Some(c.action), cardId = Option(h.id)))
              }
            case _ => Seq(ActionRequestBody(c.name, Some(c.action)))
          }))

      case _: CampFireAction => CardResponse(c.name, c.description,  c.id, mkChargesStr(c), canPlayCard(c),
        ActionRequest(c.name, c.description, s"game/${gs.uuid}/camp/${c.id}", Seq.empty))

      case _: EquipAction => CardResponse(c.name, c.description,  c.id, mkChargesStr(c), canPlayCard(c),
        ActionRequest(c.name, c.description, s"game/${gs.uuid}/equip/${c.id}",
          Seq(ActionRequestBody(c.name, action= Option(c.action), id = Option(c.id)))))
    }

    val newHand = gs.cards.hand.map(toCardResponse)
//    val newDeck = gs.cards.deck.map(toCardResponse)
//    val newDiscard = gs.cards.discard.map(toCardResponse)
//    val newPassives = gs.cards.passive.map(toCardResponse)

    val weaponResp = gs.cards.equippedCards.weapon.map(toCardResponse)
    val armourResp = gs.cards.equippedCards.armour.map(toCardResponse)
    val equipResp = EquippedCardsResponse(weaponResp, armourResp)

    val cards = CardsResponse(newHand, equipResp)

    val blockReq = ActionRequest("Block", "Block for one turn, greatly increasing defense", s"game/${gs.uuid}/block", Seq(ActionRequestBody("Block")))
    val actions: Seq[ActionRequest] = if (gs.dungeon.currentEncounter.enemies.forall(_.name == "Camp Fire")) {
      gs.cards.passive
        .withFilter(_.action.isInstanceOf[CampFireAction])
          .map(p => p.action.asInstanceOf[CampFireAction] match {
            case _: DiscardingCampFireAction =>

              val reqs = for {
                c <- gs.cards.hand
                cid = Option(c.id)
                req = ActionRequestBody(c.name, Option(p.action.asInstanceOf[CampFireAction]), cardId = cid)
              } yield req

              ActionRequest(p.name, p.description, s"game/${gs.uuid}/camp/${p.id}", reqs)
            case _: CampFireAction =>
              ActionRequest(p.name, p.description, s"game/${gs.uuid}/camp/${p.id}", Seq(ActionRequestBody(p.name, Option(p.action.asInstanceOf[CampFireAction]))))
          }) :+ blockReq
    } else {
      gs.dungeon.currentEncounter.enemies
        .map(e => ActionRequest(s"Attack ${e.name}",
          s"Use a basic attack on ${e.name}",
          s"game/${gs.uuid}/attack",
          Seq(ActionRequestBody(s"Attack ${e.name}", targetId = Some(e.id)))))
          .toSeq :+ blockReq
    }

    def hasTurn(gm: GameMessage) = gm.text.contains(" turn.")

    val msgs = if (gs.messages.count(hasTurn) <= 1) gs.messages.filterNot(hasTurn)
    else {
      gs.messages.reverse.tail.takeWhile(gm => !hasTurn(gm)).reverse
    }
    GameResponse(gs.uuid, gs.player, cards, gs.dungeon.currentEncounter, actions, msgs)
  }


  implicit class ToCardResponse(gs: GameState){
    def asResponse: GameResponse = toGameResponse(gs)
  }
}

case class GameResponse(uuid: UUID, player: Player, cards: CardsResponse, currentEncounter: Battle, actions: Seq[ActionRequest], messages: Seq[GameMessage])

case class CardsResponse(hand: Seq[CardResponse], equippedCards: EquippedCardsResponse)

case class EquippedCardsResponse(weapon: Option[CardResponse]=None, armour: Option[CardResponse]=None, jewelery: Option[CardResponse]=None)

case class CardResponse(name: String, description: String, id: UUID, charges: Option[String], playable:Boolean, action: ActionRequest)

case class ActionRequest(name: String, description: String, uri: String, request: Seq[ActionRequestBody])

case class ActionRequestBody(description:String, action: Option[Action]=None, targetId: Option[UUID]=None, targetIds: Option[Set[UUID]]=None, cardId: Option[UUID]=None, id: Option[UUID]=None)