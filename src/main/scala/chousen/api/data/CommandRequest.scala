package chousen.api.data

import java.util.UUID

sealed trait CommandRequest

case class AttackRequest(targetId: UUID) extends CommandRequest

case class SingleTargetActionRequest(targetId: UUID, actionId: ActionId) extends CommandRequest

case class MultiTargetActionRequest(targetId: Set[UUID], actionId: MultiActionId) extends CommandRequest


sealed trait ActionId

case object CrushingBlow extends ActionId
case object QuickAttack extends ActionId

sealed trait MultiActionId

case object Fireball