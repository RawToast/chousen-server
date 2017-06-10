package chousen.game.status

import chousen.api.data._

object StatusBuilder {
  def makeHaste(amount: Int, turns:Int=6) = Status(Fast, "Significantly increases the player's speed", amount, 6)
  def makeMight(amount: Int, turns:Int=6) = Status(Might, "Significantly increases the player's strength", amount, 6)
  def makeDexterity(amount: Int, turns:Int=6) = Status(Dexterity, "Significantly increases the player's dexterity", amount, 6)
  def makeSmart(amount: Int, turns:Int=6) = Status(Smart, "Significantly increases the player's intellect", amount, 6)
  def makeStoneSkin(amount: Int, turns:Int=6) = Status(StoneSkin, "Significantly increases the player's defense", amount, 6)
}
