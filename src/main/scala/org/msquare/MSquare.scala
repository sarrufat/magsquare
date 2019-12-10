

package org.msquare

import scalax.collection.Graph
import scalax.collection.edge.LUnDiEdge

import scala.collection.MapView
import scala.util.{Random, Try}

trait Square[T] {
  def order: Int

  def data: Array[T]
}

case class WeightedSquare(order: Int) extends Square[Weight] {
  val data: Array[Weight] = {
    val idat = for {
      x <- 0 until order
      y <- 0 until order
    } yield {
      2 + (if (x == y) 1 else 0) + (if (x + y + 1 == order) 1 else 0)
    }
    idat.toArray
  }
}

object MSquare {
  // The magic Constant
  def magicConstant(n: Int): Weight = n * (n * n + 1) / 2

  def possibleRCs(n: Int): List[RCD] = {
    val M = magicConstant(n)

    def xcomb(vals: List[Int], n: Int): List[RCD] = {
      if (n > vals.size) Nil
      else vals match {
        case _ :: _ if n == 1 => vals.map(List(_))
        case hd :: tl => xcomb(tl, n - 1).map(hd :: _) ::: xcomb(tl, n)
        case _ => Nil
      }
    }

    val top = n * n
    val values = List.range(1, top + 1)
    xcomb(values, n).filter(_.sum == M)
  }

  /*
   * Brute force solver with possibleRCs aid
   */
  def bfsolve(order: Int): (List[RCD], Weight, Weight) = {
    val possibles = possibleRCs(order)
    val values = List.range(1, order * order + 1)
    var attemps = 0
    var totalaAttemps = 0

    def trying() = Try({
      var vvalues = values
      totalaAttemps += 1
      val ret = for {t <- 1 to order} yield {
        //        val rcd = generateRNDRDC(possibles, List(), vvalues)
        val rcd = generateRNDRDCOpti(possibles, vvalues)
        vvalues = vvalues.filterNot(rcd.contains(_))
        rcd
      }
      attemps += 1
      ret.toList
    })

    var found = false
    var oret = List[RCD]()
    while (!found) {
      for (result <- trying()) {
        found = verifyMSquare(order, result)
        oret = result
      }
    }
    (oret, attemps, totalaAttemps)
  }

  def bfsolveCross(order: Int): (List[RCD], Weight, Weight) = {
    val possibles = possibleRCs(order)
    val values = List.range(1, order * order + 1)

    def trySsolveCrossed = {
      // Horizontal + vertical
      var vvalues = values
      var results: List[RCD] = List()
      for (idx <- 0 until order * 2 - 1) yield {
        val prefix = for (j <- 0 until idx by 2) yield {
          val col = idx / 2
          val row = (idx + 1) % 2 + j
          results(row)(col)
        }
        val rcd = generateRNDRDCOpti(possibles, prefix.toList, vvalues)
        results = results :+ rcd
        vvalues = vvalues.filterNot(rcd.contains(_))
      }
      //   println(crossRCD)
      (for (idx <- 0 until order * 2 by 2) yield results(idx)).toList
    }

    var found = false
    var oret = List[RCD]()
    var intentos = 0
    var totalIntentos = 0
    while (!found) {
      totalIntentos += 1
      for (result <- Try(trySsolveCrossed)) {
        intentos += 1
        found = verifyMSquare(order, result)
        oret = result
      }
    }
    (oret, intentos, totalIntentos)
  }

  @scala.annotation.tailrec
  def generateRNDRDC(possibles: List[RCD], actual: RCD, values: RCD): RCD = {
    val order = possibles.head.size
    // check if thare some values possible
    // if not throw excpetion
    if (possibles.filter(p => actual.forall(v => p.contains(v))).forall(poss => poss.forall(v => !values.contains(v)))) {
      throw new Exception("Not Found: " + actual)
    }
    //  println("--> " + actual + " values -> " + values)
    actual match {
      case Nil =>
        val v = values(Random.nextInt(values.size))
        generateRNDRDC(possibles, List(v), values.filterNot(_ == v))
      case _ =>
        var v = values(Random.nextInt(values.size))
        //        while (actual.exists(_ == v)) {
        //          v = values(Random.nextInt(values.size))
        //        }
        val possible = (actual :+ v).sorted
        if (possibles.exists(p => possible.forall(v => p.contains(v)))) {
          val added = actual :+ v
          if (added.size == order)
            added
          else
            generateRNDRDC(possibles, added, values.filterNot(_ == v))
        } else
          generateRNDRDC(possibles, actual, values)

    }
  }

  def generateRNDRDCOpti(possibles: List[RCD], values: RCD): RCD = {
    val actualPos = possibles.filter(p => p.forall(v => values.contains(v)))
    actualPos match {
      case Nil => throw new Exception("Not Found")
      case h :: Nil => h
      case _ => Random.shuffle(actualPos(Random.nextInt(actualPos.size)))
    }
  }

  def generateRNDRDCOpti(possibles: List[RCD], actual: RCD, values: RCD): RCD = {
    val actualPos = possibles.filter(p => actual.forall(v => p.contains(v)) && p.forall(v => values.contains(v) || actual.contains(v)))
    actualPos match {
      case Nil => throw new Exception("Not Found")
      case h :: Nil => actual ++ Random.shuffle(h.diff(actual))
      case _ => actual ++ Random.shuffle(actualPos(Random.nextInt(actualPos.size)).diff(actual))
    }
  }

  def getLRDiagonal(square: List[RCD]): Seq[Weight] = {
    for (xy <- square.indices) yield square(xy)(xy)
  }

  def getRlDiagonal(square: List[RCD]): Seq[Weight] = {
    val ord = square.size - 1
    for (xy <- square.indices) yield square(ord - xy)(xy)
  }

  def verifyMSquare(order: Int, square: List[RCD]): Boolean = {
    val s = square.flatten.distinct.size
    if (s != order * order) return false
    val sigma = magicConstant(order)
    val lr = getLRDiagonal(square)
    val rl = getRlDiagonal(square)
    square.forall {
      _.sum == sigma
    } && square.transpose.forall {
      _.sum == sigma
    } && lr.sum == sigma && rl.sum == sigma
  }

  def frequencies(sigmas: List[RCD]): MapView[Int, Int] = {
    val flatted = sigmas.flatten
    flatted.groupBy(x => x).view.mapValues(_.size)
  }

  def connectedPossibleRCs(rcds: List[RCD]): RCDGraph = {
    val rcdswidxs = rcds.zipWithIndex
    val edges = for {
      rcdwi <- rcdswidxs
      rcd2 <- rcds.drop(rcdwi._2 + 1)
      ele <- rcdwi._1
      if rcd2.contains(ele)
    } yield {
      LUnDiEdge(rcdwi._1, rcd2)(ele)
    }
    val g = Graph.from(rcds, edges)
    g
  }
}