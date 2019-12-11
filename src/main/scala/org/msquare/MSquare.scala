

package org.msquare

import java.util.concurrent.atomic.AtomicBoolean

import scala.collection.MapView
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.language.postfixOps
import scala.util.{Random, Success}

object MSquare {
  // The magic Constant
  def magicConstant(n: Int): Weight = n * (n * n + 1) / 2

  def possibleRCs(n: Int): Vector[RCD] = {
    val M = magicConstant(n)

    def xcomb(vals: Vector[Int], n: Int): Vector[RCD] = {
      if (n > vals.size) Vector()
      else if (vals.size >= 2 && n == 1)
        vals.map(Vector(_))
      else if (vals.size >= 2)
        xcomb(vals.drop(1), n - 1).map(_ :+ vals.head) ++ xcomb(vals.drop(1), n)
      else
        Vector()
    }

    val top = n * n
    val values = Vector.range(1, top + 1)
    xcomb(values, n).filter(_.sum == M)
  }


  def bfsolveCross(order: Int): (Vector[RCD], Weight, Weight) = {
    val possibles = possibleRCs(order)

    val values = Vector.range(1, order * order + 1)
    val mc = magicConstant(order)

    def generateRNDRDCOpti(actual: RCD, remainingPossibilities: RCD): RCD = {
      val actualPos = possibles.filter { possValue =>
        // Select possibles that all actual values are in some possible combination and all that's possible values are contained in remaining values or in the actual value
        actual.forall { actualValue => possValue.contains(actualValue) } && possValue.forall { pValue =>
          remainingPossibilities.contains(pValue) || actual.contains(pValue)
        }
      }
      actualPos match {
        // Any possible RFC not found
        case Vector() =>
          throw new Exception("Not Found")
        // Actual plus different found values
        case Vector(h) =>
          actual ++ Random.shuffle(h.diff(actual))
        // Actual plus one ramdom selected
        case _ =>
          actual ++ Random.shuffle(actualPos(Random.nextInt(actualPos.size)).diff(actual))
      }
    }

    def trySolveCrossed: Vector[RCD] = {
      // Horizontal + vertical
      var vvalues = values
      var results: List[RCD] = List()
      for (idx <- 0 until order * 2 - 1) {
        val prefix = for (j <- 0 until idx by 2) yield {
          val col = idx / 2
          val row = (idx + 1) % 2 + j
          results(row)(col)
        }
        val rcd = generateRNDRDCOpti(prefix.toVector, vvalues)
        results = results :+ rcd
        vvalues = vvalues.filterNot(rcd.contains(_))
      }
      //   println(crossRCD)
      (for (idx <- 0 until order * 2 by 2) yield results(idx)).toVector
    }

    val found: AtomicBoolean = new AtomicBoolean(false)
    var oret = Vector[RCD]()
    var intentos = 0
    var totalIntentos = 0
    val availableProcessors = Runtime.getRuntime.availableProcessors() * 2
    while (!found.get()) {
      totalIntentos += availableProcessors
      val futures = for (_ <- 1 to availableProcessors) yield Future {
        trySolveCrossed
      }
      for {
        fut <- futures
        // result <- fut if !found
      } {
        fut onComplete {
          case Success(result) if !found.get() =>
            intentos += 1
            found.set(verifyMSquare(order, result, mc))
            oret = result
          case _ =>
        }
      }
      for (fut <- futures) Await.ready(fut, 1000 milliseconds)

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
      case Vector() =>
        val v = values(Random.nextInt(values.size))
        generateRNDRDC(possibles, Vector(v), values.filterNot(_ == v))
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


  def getLRDiagonal(square: Vector[RCD]): Seq[Weight] = {
    for (xy <- square.indices) yield square(xy)(xy)
  }

  def getRlDiagonal(square: Vector[RCD]): Seq[Weight] = {
    val ord = square.size - 1
    for (xy <- square.indices) yield square(ord - xy)(xy)
  }

  def verifyMSquare(order: Int, square: Vector[RCD], sigma: Int): Boolean = {
    val s = square.flatten.distinct.size
    if (s != order * order) return false
    //  val sigma = magicConstant(order)
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

  def rotations(square: Vector[RCD]): Seq[Vector[RCD]] = {
    val r1 = square.reverse.transpose
    val r2 = r1.reverse.transpose
    val r3 = r2.reverse.transpose
    List(r1, r2, r3)
  }

  def reflections(square: Vector[RCD]): Seq[Vector[RCD]] = {
    // Vertical
    val r1 = square.map(_.reverse)
    // Horizonatal
    val r2 = square.reverse
    // Diagonal left top -> right bottom

    val dim = square.size
    val r3 = for (i <- 0 until dim) yield {
      for (j <- 0 until dim) yield {
        square(j)(i)
      }
    }
    val r4 = for (i <- 1 to dim) yield {
      for (j <- 1 to dim) yield {
        square(dim - j)(dim - i)
      }
    }
    List(r1, r2, r3.map(_.toVector).toVector, r4.map(_.toVector).toVector)
  }
}