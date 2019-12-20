package org.msquare

object DoublyEvenMethod {
  private val m4diagonals: Vector[RCD] = {
    Vector(Vector(1, 0, 0, 1), Vector(0, 1, 1, 0), Vector(0, 1, 1, 0), Vector(1, 0, 0, 1))
  }

  def subPartition(order: Int) = {
    val porder = order / 4
    val lines = for (_ <- 0 until porder) yield {
      val lines = for (v <- 0 until 4) yield {
        val line = for (_ <- 0 until porder) yield {
          m4diagonals(v)
        }
        line.flatten
      }
      lines.flatten
    }
    lines.flatten
  }

  val solver: MSquare.Solver = { order =>
    assert(order % 4 == 0)
    val u = 4
    val progression = 1 to order * order
    val spar = subPartition(order)
    var deleted = progression.zip(spar).filter(_._2 == 0).map(_._1).sorted
    val flatSq = (for {
      zt <- progression.zip(spar.reverse).reverse
    } yield {
      if (zt._2 == 0) {
        val dv = deleted.head
        deleted = deleted.drop(1)
        dv
      } else
        zt._1
    }).reverse.toVector
    ((for (u <- 0 until order) yield {
      flatSq.slice(u * order, u * order + order)
    }).toVector, 0, 0)
  }
}
