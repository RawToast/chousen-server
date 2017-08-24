package chousen.game.dungeon

import chousen.api.data.{Battle, Dungeon, Enemy}

trait DungeonBuilder {
  def makeDungeon(dungeonSeed:Int,
                  dungeonSeed2:Int,
                  dungeonSeed3:Int): Dungeon
}

case class BattleBuilder(battle: Battle=Battle(Set.empty[Enemy])){

  def +(e: Enemy) = {
    copy(this.battle.copy(this.battle.enemies + e))
  }
}

class SimpleDungeonBuilder() extends DungeonBuilder with EnemyBuilder {


  def makeDungeon(dungeonSeed:Int = new scala.util.Random().nextInt(6),
                  dungeonSeed2:Int = new scala.util.Random().nextInt(6),
                  dungeonSeed3:Int = new scala.util.Random().nextInt(6)) = {

    val battle1: Battle = Battle(Set(createSloth, createSlime))

    val battle2: Battle = {dungeonSeed match {
      case (0 | 1 | 2) => BattleBuilder() + createRat + createRat + createRat + createRat
      case (3 | 4) => BattleBuilder() + gnoll + createSlime
      case _ => BattleBuilder() + giantWorm + createRat
    }}.battle

    val battle3: Battle = {dungeonSeed2 match {
      case (0 | 1 | 2) => BattleBuilder() + oldOrc + createRat + createSloth + createRat + createSlime
      case (3 | 4) => BattleBuilder() + golem + gnoll
      case _ => BattleBuilder() + oldOrc + createRat + createRat
    }}.battle

    val battle4: Battle = (BattleBuilder() + orc + troll).battle
    val battle5: Battle = (BattleBuilder() + giantRat + goblin + giantRat).battle

    val battle6: Battle = {(dungeonSeed + dungeonSeed2) match {
      case 0 => BattleBuilder() + giantWorm + warrior + giantWorm
      case (1 | 2 | 3 | 4 | 5) => BattleBuilder() + gnoll + golem + gnoll
      case (6 | 7 | 8 | 9 | 10) => BattleBuilder() + giantRat + giantRat + giantRat
    }}.battle

    val battle7: Battle = {(dungeonSeed + dungeonSeed2) match {
      case 0 => BattleBuilder() + orc + orc
      case (1 | 2 | 3 | 4 | 5) => BattleBuilder() + orcFighter
      case (6 | 7 | 8 | 9 | 10) => BattleBuilder() + warrior + warrior
    }}.battle

    val battle10Left = dungeonSeed match {
      case (0 | 1 | 2) => Battle(Set(troll))
      case (3 | 4) => Battle(Set(orc))
      case _ => Battle(Set(orcFighter))
    }

    val battle10Right = {dungeonSeed2 match {
      case (0 | 1 | 2) => BattleBuilder() + troll
      case 3 => BattleBuilder() + oldOrc + oldOrc
      case 4 => BattleBuilder() + warrior + golem
      case _ => BattleBuilder() + orcFighter
    }}.battle

    val boss = {dungeonSeed3 match {
      case (0 | 1 ) => BattleBuilder() + orcWarriorS
      case (2 | 3) => BattleBuilder() + orcWarriorD
      case 4 => BattleBuilder() + orcFighter + orcFighter
      case _ => BattleBuilder() + orcWarriorQ
    }}.battle


    val battle9 = (BattleBuilder() + golem + golem).battle

    val battle8: Battle = Battle(battle10Left.enemies ++ boss.enemies ++ battle10Right.enemies)

    val finalBattle = {dungeonSeed3 match {
      case (0 | 1 ) => BattleBuilder() + orcWarriorD + orcWizard + orcWarriorQ
      case (2 | 3) => BattleBuilder() + orcWarriorS + orcWizard + orcWarriorQ
      case _ => BattleBuilder() + orcWarriorS + orcWizard + orcWarriorD
    }}.battle

    def campfire = Battle(Set(campFire))

    Dungeon(battle1, Seq(campfire,
      battle2, battle3, campfire,
      battle4, battle5, campfire,
      battle6, battle7, campfire,
      battle8, battle9, campfire,
      finalBattle
    ))
  }
}
