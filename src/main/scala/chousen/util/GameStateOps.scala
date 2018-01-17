package chousen.util

import java.util.UUID

import chousen.api.data._

object GameStateOps extends GameStateOps

trait GameStateOps extends GameResponseGenerator {

  implicit class ToCardResponse(gs: GameState){
    def asResponse: GameResponse = toGameResponse(gs, Seq.empty)
    def asResponse(diff: Seq[GameMessage]): GameResponse = toGameResponse(gs, diff)
  }

  implicit class GameStateSyntax(gs: GameState) {
    def addMessage(gameMessage: GameMessage): GameState = gs.copy(messages = gs.messages :+ gameMessage)
  }
}

trait GameResponseGenerator {
  def toGameResponse(gs: GameState, diff: Seq[GameMessage]): GameResponse = {


    def mkChargesStr(c: Card) = for {
      charges <- c.charges
      max <- c.maxCharges
    } yield s"($charges/$max)"

    def canPlayCard(c :Card):Boolean = if (c.name.contains("Essence")) {
      if (gs.cards.playedEssence) false else true
    } else true

    def toCardResponse(card: Card):CardResponse = card.action match {
      case _: SingleTargetAction => CardResponse(card.name, card.description, card.id, mkChargesStr(card), canPlayCard(card),
        ActionRequest(card.name, card.description, s"game/${gs.uuid}/single/${card.id}",
          gs.dungeon.currentEncounter.enemies.toSeq.map(e =>
            ActionRequestBody(e.name, Some(card.action), targetId = Option(e.id)))))

      case _: MultiAction => CardResponse(card.name, card.description, card.id, mkChargesStr(card), canPlayCard(card),
        ActionRequest(card.name, card.description, s"game/${gs.uuid}/multi/${card.id}",
          Seq(ActionRequestBody(card.name, Some(card.action), targetIds = Option(gs.dungeon.currentEncounter.enemies.map(_.id))))))

      case _: SelfAction => CardResponse(card.name, card.description,  card.id, mkChargesStr(card), canPlayCard(card),
        ActionRequest(card.name, card.description, s"game/${gs.uuid}/self/${card.id}",
          Seq(ActionRequestBody(card.name, Some(card.action)))))

      case ca: CardAction => {
        val ars = ActionRequest(card.name, card.description, s"game/${gs.uuid}/card/${card.id}",
          ca match {
            case action: DiscardCardAction =>
              action match {
                case ReduceRequirements => gs.cards.hand
                  .filter(r => r.requirements.str.nonEmpty || r.requirements.dex.nonEmpty || r.requirements.int.nonEmpty)
                  .map(h => ActionRequestBody(h.name, Some(card.action), cardId = Option(h.id)))
                case IncreaseCharges =>
                  gs.cards.hand
                    .filter(_.charges.nonEmpty)
                    .map(h => ActionRequestBody(h.name, Some(card.action), cardId = Option(h.id))) ++
                  gs.cards.equippedCards.skills
                    .filter(_.charges.nonEmpty)
                    .map(h => ActionRequestBody(h.name, Some(card.action), cardId = Option(h.id)))
                case _ => gs.cards.hand
                  .filter(_.id != card.id)
                  .map(h => ActionRequestBody(h.name, Some(card.action), cardId = Option(h.id)) )
              }
            case std: StandardCardAction =>
              std match {
                case AnotherTime => gs.cards.discard
                    .foldLeft(Seq.empty[Card])((cs, c) => if (cs.exists(_.action == c.action)) cs else cs :+ c)
                  .map(c => ActionRequestBody(c.name, Some(card.action), cardId = Option(c.id)))
                case FindersKeepers => gs.cards.deck
                  .foldLeft(Seq.empty[Card])((cs, c) =>
                    if (cs.exists(_.action == c.action)) cs
                    else if (!(c.action.isInstanceOf[CardAction] || c.action.isInstanceOf[EquipAction]))  cs :+ c
                    else cs).sortBy(_.name)
                  .map(c => ActionRequestBody(c.name, Some(card.action), cardId = Option(c.id)))

                case PickACard => gs.cards.deck
                  .foldLeft(Seq.empty[Card])((cs, c) =>
                    if (cs.exists(_.action == c.action)) cs
                    else if (c.action.isInstanceOf[CardAction])  cs :+ c
                    else cs).sortBy(_.name)
                  .map(c => ActionRequestBody(c.name, Some(card.action), cardId = Option(c.id)))
                case _ => Seq(ActionRequestBody(card.name, Some(card.action)))
              }
          })

        CardResponse(card.name, card.description, card.id, mkChargesStr(card), ars.request.nonEmpty, ars)
      }
      case _: CampFireAction =>  CardResponse(card.name, card.description,  card.id, mkChargesStr(card), canPlayCard(card),
                    ActionRequest(card.name, card.description, s"game/${gs.uuid}/camp/${card.id}", Seq.empty))

      case _: EquipAction => CardResponse(card.name, card.description,  card.id, mkChargesStr(card), canPlayCard(card),
        ActionRequest(card.name, card.description, s"game/${gs.uuid}/equip/${card.id}",
          Seq(ActionRequestBody(card.name, action= Option(card.action), id = Option(card.id)))))
    }

    def toShortCardResponse(c: Card) = ShortCardResponse(c.name, c.id, c.action)

    val newHand = gs.cards.hand.map(toCardResponse)
    val newDeck = gs.cards.deck.map(toShortCardResponse)
      .foldLeft(Seq.empty[ShortCardResponse])((cs, cr) => if (!cs.exists(_.name == cr.name)) cs :+ cr else cs)
    val newDiscard = gs.cards.discard.map(toShortCardResponse)
      .foldLeft(Seq.empty[ShortCardResponse])((cs, cr) => if (!cs.exists(_.name == cr.name)) cs :+ cr else cs)

    val weaponResp = gs.cards.equippedCards.weapon.map(toCardResponse)
    val armourResp = gs.cards.equippedCards.armour.map(toCardResponse)
    val skillResp = gs.cards.equippedCards.skills.map(toCardResponse)
    val equipResp = EquippedCardsResponse(weaponResp, armourResp, skills = skillResp)

    val cards = CardsResponse(newHand, equipResp, newDeck, newDiscard)

    val blockReq = ActionRequest("Block", "Block for one turn, greatly increasing defense", s"game/${gs.uuid}/block", Seq(ActionRequestBody("Block")))
    val actions: Seq[ActionRequest] = if (gs.dungeon.currentEncounter.enemies.forall(_.name == "Camp Fire")) {
      gs.cards.passive
        .withFilter(_.action.isInstanceOf[CampFireAction])
        .map(p => p.action.asInstanceOf[CampFireAction] match {
          case dcfa: DiscardingCampFireAction =>

            def learnFilter(c: Card): Boolean = c.charges.nonEmpty || p.action != LearnSkill

            val requirements = for {
              c <- gs.cards.hand.filter(learnFilter)
              cid = Option(c.id)
              req = ActionRequestBody(c.name, Option(p.action.asInstanceOf[CampFireAction]), cardId = cid)
            } yield req

            ActionRequest(p.name, p.description, s"game/${gs.uuid}/camp/${p.id}", requirements)
          case _: CampFireAction =>
            ActionRequest(p.name, p.description, s"game/${gs.uuid}/camp/${p.id}",
              Seq(ActionRequestBody(p.name, Option(p.action.asInstanceOf[CampFireAction]))))
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

    val msgs: Seq[String] = gs.messages.drop(diff.size).filterNot(hasTurn).map(_.text)

    GameResponse(gs.uuid, gs.player, cards, gs.dungeon.currentEncounter, actions, msgs)
  }
}

case class GameResponse(uuid: UUID, player: Player, cards: CardsResponse, currentEncounter: Battle, actions: Seq[ActionRequest], messages: Seq[String])

case class CardsResponse(hand: Seq[CardResponse], equippedCards: EquippedCardsResponse, inDeck: Seq[ShortCardResponse], inDiscard: Seq[ShortCardResponse])
case class ShortCardResponse(name: String, id: UUID, action: Action)

case class EquippedCardsResponse(weapon: Option[CardResponse]=None, armour: Option[CardResponse]=None, jewelery: Option[CardResponse]=None, skills: Seq[CardResponse])

case class CardResponse(name: String, description: String, id: UUID, charges: Option[String], playable:Boolean, action: ActionRequest)

case class ActionRequest(name: String, description: String, uri: String, request: Seq[ActionRequestBody])

case class ActionRequestBody(description:String, action: Option[Action]=None, targetId: Option[UUID]=None, targetIds: Option[Set[UUID]]=None, cardId: Option[UUID]=None, id: Option[UUID]=None)