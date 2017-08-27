package chousen.game.dungeon

import java.util.UUID

import chousen.api.data.{CharStats, Enemy}

object EnemyBuilder extends EnemyBuilder

trait EnemyBuilder {
  private def mkEnemy(name: String, stats: CharStats) = Enemy(name, UUID.randomUUID(), stats, 0)

  def campFire = Enemy("Camp Fire", UUID.randomUUID(), CharStats(3, 3, strength = 6, speed = 0), 0)

  // T1
  def createSlime: Enemy = mkEnemy("Slime", CharStats(13, 13, strength = 9, vitality = 6))
  def createSloth: Enemy = mkEnemy("Sloth", CharStats(26, 26, strength = 16, dexterity = 4, vitality = 6, speed = 4))
  def createRat: Enemy = mkEnemy("Rat", CharStats(7, 7, strength = 4, dexterity = 6, vitality = 4, speed = 12))

  // T2
  def gnoll: Enemy = mkEnemy("Gnoll", CharStats(35, 35, strength = 15, dexterity = 8, vitality = 8))
  def giantWorm: Enemy = mkEnemy("Giant Worm", CharStats(50, 50, strength = 22, vitality = 6, speed = 4))


  // T3
  def giantRat: Enemy = mkEnemy("Giant Rat", CharStats(26, 26, dexterity = 10, vitality = 6, speed = 11))
  def oldOrc: Enemy = mkEnemy("Old Orc", CharStats(70, 70, strength = 28, dexterity = 2, vitality = 10, speed = 6))
  def goblin: Enemy = mkEnemy("Goblin", CharStats(50, 50, strength = 7, dexterity = 22, vitality = 9, speed = 9))
  def golem: Enemy = mkEnemy("Steam Golem", CharStats(100, 100, strength = 30, dexterity = 4, vitality = 16, speed = 3))


  // T4
  def warrior: Enemy = mkEnemy("Warrior", CharStats(70, 70, strength = 27, dexterity = 17, vitality = 25))
  def orc: Enemy = mkEnemy("Orc", CharStats(90, 90, strength = 32, dexterity = 7, vitality = 15, speed = 7))
  def troll: Enemy = mkEnemy("Troll", CharStats(160, 160, strength = 50, dexterity = 4, intellect = 5, vitality = 14, speed = 3))
  def kobold: Enemy = mkEnemy("Kobold", CharStats(45, 45, strength = 17, dexterity = 24, intellect = 10, vitality = 12, speed = 10))

  // T5
  def orcFighter: Enemy = mkEnemy("Orc Fighter", CharStats(100, 100, strength = 35, dexterity = 25, vitality = 16, speed = 7))


  def orcWarriorS: Enemy = mkEnemy("Orc Warrior", CharStats(130, 130, strength = 42, dexterity = 10, vitality = 20))
  def orcWarriorQ: Enemy = mkEnemy("Orc Warrior", CharStats(125, 125, strength = 38, dexterity = 14, vitality = 15, speed = 9))
  def orcWarriorD: Enemy = mkEnemy("Orc Warrior", CharStats(135, 135, strength = 40, dexterity = 10, vitality = 30, speed = 7))

  def knollShaman: Enemy = mkEnemy("Knoll Shaman", CharStats(40, 40, strength = 12, dexterity = 9, vitality = 7, intellect = 13)).copy(position = 20)
  def gKnollShaman: Enemy = mkEnemy("Great Knoll Shaman", CharStats(70, 70, strength = 15, dexterity = 11, vitality = 13, intellect = 13)).copy(position = 35)
  def orcPriest: Enemy = mkEnemy("Old Orkish Priest", CharStats(50, 50, strength = 18, dexterity = 7, vitality = 12, intellect = 16)).copy(position = 20)
  def orcWizard: Enemy = mkEnemy("Orkish Wizard", CharStats(90, 90, strength = 31, dexterity = 14, vitality = 17, intellect = 20)).copy(position = 50)
  def orcGPriest: Enemy = mkEnemy("Orkish Grand Priest", CharStats(320, 320, strength = 31, dexterity = 14, vitality = 37, intellect = 20)).copy(position = 50)

  def ratKing: Enemy = mkEnemy("Rat King", CharStats(120, 120, strength = 18, dexterity = 18, vitality = 13, intellect = 13, speed = 10)).copy(position = -150)
  def hugeGolem: Enemy = mkEnemy("Huge Steam Golem", CharStats(240, 240, strength = 42, dexterity = 10, vitality = 30, speed = 2))
  def draconian: Enemy = mkEnemy("Draconian", CharStats(180, 180, strength = 40, dexterity = 26, vitality = 30, intellect = 22, speed = 9))
  def smallOrc: Enemy = mkEnemy("Tiny Orc", CharStats(33, 33, strength = 25, dexterity = 7, vitality = 13, speed = 11))
  def tripleOrc: Enemy = mkEnemy("TripleOrc", CharStats(333, 333, strength = 60, dexterity = 24, vitality = 33, intellect = 22, speed = 7))
  def fireOrcKing: Enemy = mkEnemy("Ancient Fire Orc", CharStats(600, 600, strength = 80, dexterity = 50, vitality = 50, intellect = 50)).copy(position = -50)
  def orcKing: Enemy = mkEnemy("Orc King", CharStats(1000, 1000, strength = 150, dexterity = 150, vitality = 50, intellect = 50, speed = 7)).copy(position = -1000)
  def kraken: Enemy = mkEnemy("Kraken", CharStats(480, 480, strength = 35, dexterity = 35, vitality = 35, intellect = 12, speed = 17))

  def totem: Enemy = mkEnemy("Totem", CharStats(100, 100, strength = 1, dexterity = 1, vitality = 10, intellect = 1, speed = 10)).copy(position = 25)
}

