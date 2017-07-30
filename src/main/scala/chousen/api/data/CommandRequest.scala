package chousen.api.data

import java.util.UUID

sealed trait CommandRequest

case class AttackRequest(targetId: UUID) extends CommandRequest

case class SelfInflictingActionRequest(action: SelfAction) extends CommandRequest

case class SingleTargetActionRequest(targetId: UUID, action: SingleTargetAction) extends CommandRequest

case class MultiTargetActionRequest(targetId: Set[UUID], action: MultiAction) extends CommandRequest

case class CardActionRequest(action: CardAction) extends CommandRequest

case class CampfireActionRequest(action: CampFireAction) extends CommandRequest


sealed trait Action

sealed trait SingleTargetAction extends Action
sealed trait MultiAction extends Action
sealed trait SelfAction extends Action
sealed trait CardAction extends Action
sealed trait CampFireAction extends Action



case object CrushingBlow extends SingleTargetAction
case object Hamstring extends SingleTargetAction
case object StunningStrike extends SingleTargetAction
case object Counter extends SingleTargetAction
case object Destruction extends SingleTargetAction

//case object QuickAttack extends SingleTargetAction
//case object Assassinate extends SingleTargetAction
//case object TripleStrike extends SingleTargetAction


//case object Pain extends SingleTargetAction
//case object MagicMissile extends SingleTargetAction
//case object Drain extends SingleTargetAction


//case object Fireball extends MultiAction
//case object StaticField extends MultiAction
//case object MassDrain extends MultiAction
//case object Shatter extends MultiAction

case object GroundStrike extends MultiAction

//case object WindStrike extends MultiAction

case object HealWounds extends SelfAction
case object Haste extends SelfAction

case object PotionOfMight extends SelfAction
case object PotionOfDexterity extends SelfAction
case object PotionOfIntelligence extends SelfAction
case object PotionOfStoneSkin extends SelfAction

case object ElixirOfStrength extends SelfAction
case object ElixirOfDexterity extends SelfAction
case object ElixirOfIntelligence extends SelfAction
case object ElixirOfVitality extends SelfAction
case object RarePepe extends SelfAction
case object QuickStep extends SelfAction

case object Rummage extends CardAction
case object Miracle extends CardAction
case object Replace extends CardAction
case object Restore extends CardAction


case object Rest extends CampFireAction
case object Explore extends CampFireAction
case object RestAndExplore extends CampFireAction