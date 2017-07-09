package chousen

import chousen.api.types.{EqualityInstances, EqualitySyntax, MonoidInstances}

object Implicits extends Instances with Syntax

trait Instances extends EqualityInstances with MonoidInstances

trait Syntax extends EqualitySyntax