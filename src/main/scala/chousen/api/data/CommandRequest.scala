package chousen.api.data

import java.util.UUID

sealed trait CommandRequest

case class AttackRequest(targetId: UUID) extends CommandRequest

case class SingleTargetActionRequest(targetId: UUID, actionId: Int) extends CommandRequest

case class MultiTargetActionRequest(targetId: Set[UUID], actionId: Int) extends CommandRequest