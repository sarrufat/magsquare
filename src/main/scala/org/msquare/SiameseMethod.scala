package org.msquare

object SiameseMethod {
  def solver(progressionBase: Int): MSquare.Solver = { order =>
    assert(order % 2 == 1)
    val progression = (progressionBase to (order * order)).toSeq

    val serie = for {
      j <- 0 until order * order
    } yield {
      val n = progression(j)
      val jj = ((order - (j % order)) + 2 * (j / order)) % order
      val kk = (order / 2 + j - (j / order)) % order

      (jj, kk, n)
    }
    val result = serie.sortBy(t => (t._1, t._2))
    ((for (j <- 0 until order) yield {
      result.drop(j * order).take(order).map(_._3).toVector
    }).toVector, 0, 0)
  }
}
