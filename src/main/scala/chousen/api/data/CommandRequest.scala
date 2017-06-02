package chousen.api.data

import java.util.UUID

sealed trait CommandRequest

case class AttackRequest(targetId: UUID) extends CommandRequest

case class SelfInflictingActionRequest(action: SelfAction) extends CommandRequest

case class SingleTargetActionRequest(targetId: UUID, action: SingleTargetAction) extends CommandRequest

case class MultiTargetActionRequest(targetId: Set[UUID], action: MultiAction) extends CommandRequest


sealed trait Action

sealed trait SingleTargetAction extends Action
sealed trait MultiAction extends Action
sealed trait SelfAction extends Action


case object CrushingBlow extends SingleTargetAction
case object Hamstring extends SingleTargetAction
case object StunningStrike extends SingleTargetAction

case object QuickAttack extends SingleTargetAction
case object Assassinate extends SingleTargetAction
case object TripleStrike extends SingleTargetAction

case object Pain extends SingleTargetAction
case object MagicMissile extends SingleTargetAction
case object Drain extends SingleTargetAction





case object Fireball extends MultiAction
case object StaticField extends MultiAction
case object Shatter extends MultiAction
case object GroundStrike extends MultiAction
case object WindStrike extends MultiAction


case object HealWounds extends SelfAction
case object ElixirOfStrength extends SelfAction
case object ElixirOfDexterity extends SelfAction
case object ElixirOfIntelligence extends SelfAction
case object ElixirOfVitality extends SelfAction
case object RarePepe extends SelfAction
case object QuickStep extends SelfAction
