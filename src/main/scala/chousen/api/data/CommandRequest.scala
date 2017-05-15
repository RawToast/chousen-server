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
case object QuickAttack extends SingleTargetAction


case object Fireball extends MultiAction


case object HealWounds extends SelfAction
