package chousen.game.dungeon

import java.util.UUID

import chousen.api.data.{Battle, CharStats, Enemy}

object EnemyBuilder extends EnemyBuilder

trait EnemyBuilder {
  private def mkEnemy(name: String, stats: CharStats) = Battle(Set(Enemy(name, UUID.randomUUID(), stats, 0)))

  def campFire = Battle(Set(Enemy("Camp Fire", UUID.randomUUID(), CharStats(3, 3, strength = 6, speed = 0), 0)))

  // T1
  def createSlime = mkEnemy("Slime", CharStats(13, 13, vitality = 6))
  def createSloth = mkEnemy("Sloth", CharStats(26, 26, strength = 12, vitality = 4, speed = 4))
  def createRat = mkEnemy("Rat", CharStats(7, 7, strength = 4, vitality = 4, speed = 12))

  // T2
  def gnoll = mkEnemy("Gnoll", CharStats(35, 35, strength = 8, dexterity = 8, vitality = 8))
  def giantWorm = mkEnemy("Giant Worm", CharStats(50, 50, strength = 15, vitality = 6, speed = 3))


  // T3
  def giantRat = mkEnemy("Giant Rat", CharStats(26, 26, dexterity = 9, vitality = 6, speed = 11))
  def oldOrc = mkEnemy("Old Orc", CharStats(70, 70, strength = 14, dexterity = 6, vitality = 10, speed = 4))
  def goblin = mkEnemy("Goblin", CharStats(50, 50, strength = 9, dexterity = 10, vitality = 9, speed = 9))
  def golem = mkEnemy("Golem", CharStats(100, 100, strength = 17, dexterity = 4, vitality = 15, speed = 2))


  // T4
  def warrior = mkEnemy("Warrior", CharStats(65, 65, strength = 17, dexterity = 10, vitality = 25))
  def orc = mkEnemy("Orc", CharStats(85, 85, strength = 22, dexterity = 7, vitality = 15, speed = 7))
  def troll = mkEnemy("Troll", CharStats(160, 160, strength = 42, intellect = 5, vitality = 14, speed = 2))

  // T5
  def orcPrince = mkEnemy("Orc Prince", CharStats(100, 100, strength = 25, vitality = 16, speed = 7))


  def orcKing1 = mkEnemy("Orc King", CharStats(130, 130, strength = 32, vitality = 20))
  def orcKing2 = mkEnemy("Orc King", CharStats(125, 125, strength = 28, vitality = 15, speed = 10))
  def orcKing3 = mkEnemy("Orc King", CharStats(135, 135, strength = 30, vitality = 30, speed = 7))
}
