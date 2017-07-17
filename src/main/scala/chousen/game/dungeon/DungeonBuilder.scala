package chousen.game.dungeon

import chousen.api.data.Dungeon

trait DungeonBuilder {
  def makeDungeon(dungeonSeed:Int,
                  dungeonSeed2:Int,
                  dungeonSeed3:Int): Dungeon
}


class SimpleDungeonBuilder() extends DungeonBuilder with EnemyBuilder {


  def makeDungeon(dungeonSeed:Int = new scala.util.Random().nextInt(6),
                  dungeonSeed2:Int = new scala.util.Random().nextInt(6),
                  dungeonSeed3:Int = new scala.util.Random().nextInt(6)) = {
    import cats.implicits._
    import chousen.Implicits._

    val battle1 = createSloth

    val battle2 = dungeonSeed match {
      case (0 | 1 | 2) => createRat |+| createRat |+| createRat |+| createRat
      case (3 | 4) => createSlime |+| createSlime
      case _ => giantWorm
    }

    val battle3 = createSlime |+| campFire

    val battle4 = dungeonSeed2 match {
      case (0 | 1 | 2) => oldOrc |+| createRat |+| createSloth |+| createRat |+| createSlime
      case (3 | 4) => gnoll |+| gnoll
      case _ => oldOrc |+| createRat |+| createRat
    }

    val battle5 = orc |+| troll
    val battle6 = giantRat |+| goblin |+| giantRat

    val battle7 = (dungeonSeed + dungeonSeed2) match {
      case 0 => giantWorm |+| warrior |+| giantWorm
      case (1 | 2 | 3 | 4 | 5) => gnoll |+| golem |+| gnoll
      case (6 | 7 | 8 | 9 | 10) => createRat |+| giantRat |+| createRat
    }

    val battle8 = (dungeonSeed + dungeonSeed2) match {
      case 0 => orc |+| orc
      case (1 | 2 | 3 | 4 | 5) => orcPrince
      case (6 | 7 | 8 | 9 | 10) => warrior |+| warrior
    }

    val battle10Left = dungeonSeed match {
      case (0 | 1 | 2) => troll
      case (3 | 4) => orc
      case _ => orcPrince
    }
    val battle10Right = dungeonSeed2 match {
      case (0 | 1 | 2) => troll
      case 3 => oldOrc |+| oldOrc
      case 4 => warrior |+| golem
      case _ => orcPrince
    }
    val boss = dungeonSeed3 match {
      case (0 | 1 ) => orcKing1
      case (2 | 3) => orcKing3
      case 4 => orcPrince |+| orcPrince
      case _ => orcKing2
    }


    val battle9 = battle10Left |+| boss |+| battle10Right

    Dungeon(battle1, Seq(battle2, battle3, battle4, campFire,
      battle5, battle6, campFire,
      battle7, battle8, campFire,
      battle9))
  }
}
