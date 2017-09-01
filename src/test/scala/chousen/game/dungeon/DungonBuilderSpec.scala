package chousen.game.dungeon

import org.scalatest.WordSpec

class DungonBuilderSpec extends WordSpec {

  "DungeonBuilder" should {

    val builder = new SimpleDungeonBuilder()

    "Build differently when provided with different seeds" in {

      val d1 = builder.makeDungeon(0, 0, 0)
      val d2 = builder.makeDungeon(3, 3, 3)
      val d3 = builder.makeDungeon(4, 4, 4)
      val d4 = builder.makeDungeon(5, 5, 5)

      assert(d1 != d2)
      assert(d1 != d3)
      assert(d1 != d4)
      assert(d2 != d3)
      assert(d3 != d4)
    }
  }
}
