package chousen.api.data

import java.util.UUID

sealed trait CommandRequest

case class AttackRequest(targetId: UUID) extends CommandRequest

case class SelfInflictingActionRequest(actionId: SelfAction) extends CommandRequest

case class SingleTargetActionRequest(targetId: UUID, actionId: SingleTargetAction) extends CommandRequest

case class MultiTargetActionRequest(targetId: Set[UUID], actionId: MultiActionId) extends CommandRequest


sealed trait Action

sealed trait SingleTargetAction extends Action
case object CrushingBlow extends SingleTargetAction
case object QuickAttack extends SingleTargetAction


sealed trait MultiActionId extends Action
case object Fireball extends MultiActionId


sealed trait SelfAction extends Action
case object HealWounds extends SelfAction
