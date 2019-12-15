package org.msquare

/**
 * The Strachey method for magic squares is an algorithm for generating magic squares of singly even order 4k + 2
 */
object StracheyMethod {
  val solver: MSquare.Solver = { order =>
    assert(order % 4 == 2)
    val k = order / 4
    val u = order / 2
    val a = SiameseMethod.solver(1)(u)._1
    val d = SiameseMethod.solver(order * order / 4 + 1)(u)._1
    val b = SiameseMethod.solver(2 * order * order / 4 + 1)(u)._1
    val c = SiameseMethod.solver(3 * order * order / 4 + 1)(u)._1
    val a2 = c.map(_.take(k)).zip(a).map(tup => tup._1 ++ tup._2.drop(k))
    val c2 = a.map(_.take(k)).zip(c).map(tup => tup._1 ++ tup._2.drop(k))
    val b2 = d.map(_.reverse.take(k - 1).reverse).zip(b).map(tup => tup._2.take(u-(k-1)) ++ tup._1 )
    val d2 = b.map(_.reverse.take(k - 1).reverse).zip(d).map(tup =>  tup._2.take(u-(k-1)) ++ tup._1)
    val a3 = a2.zipWithIndex.map { tup =>
      if (tup._2 == u / 2)
        tup._1.updated(0, c2(u / 2)(0)).updated(u / 2, c2(u / 2)(u / 2))
      else
        tup._1
    }

    val c3 = c2.zipWithIndex.map { tup =>
      if (tup._2 == u / 2)
        tup._1.updated(0, a2(u / 2)(0)).updated(u / 2, a2(u / 2)(u / 2))
      else
        tup._1
    }

    (a3.zip(b2).map(tup => tup._1 ++ tup._2) ++ c3.zip(d2).map(tup => tup._1 ++ tup._2),
      0, 0)
  }
}
