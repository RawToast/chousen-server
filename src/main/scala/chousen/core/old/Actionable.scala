package chousen.core.old

trait Actionable[T] {
  def affect(g:T): T
}
