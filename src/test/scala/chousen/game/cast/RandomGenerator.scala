package chousen.game.cast

import scala.util.Random

case class RandomResult(result: Int, rnd: Random)


case class RandomGenerator(seed:Long) {
  val rnd = new Random(seed)

  def gimmeFour: Seq[Int] = {
    Seq(rnd.nextInt(), rnd.nextInt(), rnd.nextInt(), rnd.nextInt())
  }
}


trait RNG {
  def nextInt: (Int, RNG)
}


object RNG {
  def simple(seed: Long): RNG = new RNG {
    def nextInt: (Int, RNG) = {
      val seed2 = (seed*0x5DEECE66DL + 0xBL) &
        ((1L << 48) - 1)
      ((seed2 >>> 16).asInstanceOf[Int],
        simple(seed2))
    }
  }

  implicit val toResult: ((Int, RNG)) => Int = (x:(Int, RNG)) => x._1

  implicit val nextRng: ((Int, RNG)) => RNG = (x:(Int, RNG)) => x._2
}