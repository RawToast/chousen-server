package chousen.game.status

import chousen.api.data._

object StatusBuilder {
  def makeHaste(amount: Int, turns:Int=7) = Status(Fast, "Significantly increases the player's speed", turns, Option(amount))
  def makeMight(amount: Int, turns:Int=7) = Status(Might, "Significantly increases the player's strength", turns, Option(amount))
  def makeDexterity(amount: Int, turns:Int=7) = Status(Dexterity, "Significantly increases the player's dexterity", turns, Option(amount))
  def makeSmart(amount: Int, turns:Int=7) = Status(Smart, "Significantly increases the player's intellect", turns, Option(amount))
  def makeStoneSkin(amount: Int, turns:Int=7) = Status(StoneSkin, "Significantly increases the player's defense", turns, Option(amount))
  def makeBerserk(amount: Int, turns:Int=6) = Status(Rage, "Berserker rage", turns, Option(amount))
  def makeSlow(amount: Int, turns:Int=4) = Status(Slow, "Reduces the affected's speed", turns, Option(amount))
  def makeBlock(amount: Int=0, turns:Int=0) = Status(Block, "Reduces damage", turns, Option(amount))
  def makeFort(amount: Int=0, turns:Int=0) = Status(Fort, "Reduces damage", turns, Option(amount))
  def makeRegen(amount: Int=0, turns:Int=6) = Status(Regen, "Regenerate health", turns, Option(amount))
  def makeBurn(amount: Int, turns:Int=3) = Status(Burn, "Take damage on each turn from Burns", turns, Option(amount))
  def makePoison(amount: Int, turns:Int=4) = Status(Poison, "Take damage on each turn from Burns", turns, Option(amount))
  def makeTree(amount: Int, turns:Int=7) = Status(Tree, "Reduces speed, but increases strength, armour, and regen.", turns, Option(amount))
  def makeFear(amount: Int, turns:Int=6) = Status(Fear, "May run away on low health", turns, Option(amount))
}
