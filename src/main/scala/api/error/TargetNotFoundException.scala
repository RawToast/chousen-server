package api.error

import java.util.UUID

case class TargetNotFoundException(message: String = "Target ID was not found", cause: Throwable = null)
  extends Exception(message, cause)

object TargetNotFoundException {
  def raise(id: UUID, targets:Set[UUID]) = TargetNotFoundException(s"Target ID: $id not found, possible ids: $targets")
}