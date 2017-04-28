package chousen.util

import monocle.Lens


object LensUtil {
  def triLens[S, A, B, C](lsa : Lens[S, A], lsb : Lens[S, B], lsc: Lens[S, C]) : Lens[S, (A, B, C)] =
    Lens.apply[S, (A, B, C)](s => (lsa.get(s), lsb.get(s), lsc.get(s)))(t => lsa.set(t._1).andThen(lsb.set(t._2)).andThen(lsc.set(t._3)))
}
