package org.msquare

/**
 * The Strachey method for magic squares is an algorithm for generating magic squares of singly even order 4k + 2
 */
object StracheyMethod {

  val solver:  MSquare.Solver = { order =>
    assert(order % 4 == 2)
    val subgridOrder = order / 2

    ???
  }
}
