package chousen

import chousen.api.data.CharacterOptics
import chousen.game.core.GameStateOptics

object Optics extends AllOptics

trait AllOptics extends ApiOptics with GameOptics
trait ApiOptics extends CharacterOptics
trait GameOptics extends GameStateOptics