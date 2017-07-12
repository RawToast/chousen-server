package chousen.game.dungeon

import chousen.api.data.Dungeon


class DungeonBuilder() extends EnemyBuilder {


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

    val battle5 = dungeonSeed match {
      case (0 | 2 | 4) => golem |+| gnoll
      case (1 | 3) => giantWorm |+| giantWorm |+| createSloth
      case _ => warrior
    }

    val battle6 = dungeonSeed3 match {
      case (1 | 2 | 3) => orc |+| troll
      case (4 | 5) => golem |+| troll |+| golem
      case _ => warrior |+| troll
    }

    val battle8 = dungeonSeed2 match {
      case (5 | 4 | 3) => giantRat |+| goblin |+| giantRat
      case (2 | 1) => giantRat |+| giantRat |+| giantRat
      case _ => giantRat |+| golem |+| giantRat
    }

    val battle9 = dungeonSeed match {
      case (0 | 1 | 2) => warrior
      case (3 | 4) => orcPrince
      case _ => warrior |+| warrior
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

    val battle10 = battle10Left |+| boss |+| battle10Right

    Dungeon(battle1, Seq(battle2, battle3, battle4, campFire,
      battle5, battle6, campFire,
      battle8, campFire,
      battle9, campFire,
      battle10))
  }
}
