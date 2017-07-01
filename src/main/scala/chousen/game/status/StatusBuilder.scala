package chousen.game.status

import chousen.api.data._

object StatusBuilder {
  def makeHaste(amount: Int, turns:Int=6) = Status(Fast, "Significantly increases the player's speed", turns, Option(amount))
  def makeMight(amount: Int, turns:Int=6) = Status(Might, "Significantly increases the player's strength", turns, Option(amount))
  def makeDexterity(amount: Int, turns:Int=6) = Status(Dexterity, "Significantly increases the player's dexterity", turns, Option(amount))
  def makeSmart(amount: Int, turns:Int=6) = Status(Smart, "Significantly increases the player's intellect", turns, Option(amount))
  def makeStoneSkin(amount: Int, turns:Int=6) = Status(StoneSkin, "Significantly increases the player's defense", turns, Option(amount))
}
