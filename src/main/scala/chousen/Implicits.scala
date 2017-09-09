package chousen

import chousen.api.types.{EqualityInstances, EqualitySyntax, MonoidInstances}
import chousen.util.GameStateOps

object Implicits extends Instances with Syntax
object ChousenImplicits extends ChousenImplicits

trait ChousenImplicits extends GameStateOps

trait Instances extends EqualityInstances with MonoidInstances

trait Syntax extends EqualitySyntax