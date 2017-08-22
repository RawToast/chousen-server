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
  def warrior: Enemy = mkEnemy("Warrior", CharStats(70, 70, strength = 22, dexterity = 10, vitality = 25))
  def orc: Enemy = mkEnemy("Orc", CharStats(90, 90, strength = 30, dexterity = 6, vitality = 15, speed = 7))
  def troll: Enemy = mkEnemy("Troll", CharStats(160, 160, strength = 46, dexterity = 4, intellect = 5, vitality = 14, speed = 3))

  // T5
  def orcFighter: Enemy = mkEnemy("Orc Fighter", CharStats(100, 100, strength = 27, dexterity = 25, vitality = 16, speed = 7))


  def orcWarriorS: Enemy = mkEnemy("Orc Warrior", CharStats(130, 130, strength = 37, vitality = 20))
  def orcWarriorQ: Enemy = mkEnemy("Orc Warrior", CharStats(125, 125, strength = 32, dexterity = 12, vitality = 15, speed = 9))
  def orcWarriorD: Enemy = mkEnemy("Orc Warrior", CharStats(135, 135, strength = 35, vitality = 30, speed = 7))


  def orcWizard: Enemy = mkEnemy("Orkish Wizard", CharStats(90, 90, strength = 20, vitality = 14, intellect = 15))
}
