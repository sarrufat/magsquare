package org.msquare

object DoublyEvenMethod {

  private def replaceDiagonals(order: Int, sq: Seq[Int]): Seq[Weight] = {
    val u = order / 2
    sq.zipWithIndex.map { tup =>
      val mdiag = tup._2 % (u + 1) == 0
      val sdiag = tup._2 % (u - 1) == 0
      if (mdiag || sdiag)
        order * order + 1 - tup._1
      else
        tup._1
    }
  }
  private def toVec(u:Int, seq: Seq[Int]): Vector[Vector[Int]] = (for (j <- 0 until u) yield seq.toVector.slice(j * u, j * u + u)).toVector

  val solver: MSquare.Solver = { order =>
    assert(order % 4 == 0)
    val u = order / 2
    val progression = 1 to order * order
    val aSub = replaceDiagonals(order, progression.filter(x => (x - 1) < order * order / 2 && (x - 1) / u % 2 == 0))
    val bSub = replaceDiagonals(order, progression.filter(x => (x - 1) < order * order / 2 && (x - 1) / u % 2 == 1))
    val cSub = replaceDiagonals(order, progression.filter(x => x > order * order / 2 && (x - 1) / u % 2 == 0))
    val dSub = replaceDiagonals(order, progression.filter(x => x > order * order / 2 && (x - 1) / u % 2 == 1))
    (toVec(u, aSub).zip(toVec(u, bSub)).map( tup => tup._1 ++ tup._2) ++ toVec(u, cSub).zip(toVec(u, dSub)).map( tup => tup._1 ++ tup._2),
      0,0)
  }
}
