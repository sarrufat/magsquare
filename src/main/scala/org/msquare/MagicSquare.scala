package org.msquare

import org.msquare.MSquare.{Solver, bfsolveCross}
import org.rogach.scallop.{ScallopConf, ScallopOption}
import org.scalameter._

class Conf(arguments: Seq[String]) extends ScallopConf(arguments) {
  val order: ScallopOption[Weight] = opt[Int](required = true, name = "order")
  verify()
}

object MagicSquare extends App {
  val conf = new Conf(args)

  val order = conf.order.toOption.get

  def getSolverByMethod: Solver = {
    if (order % 2 == 1)
      SiameseMethod.solver(1)
    else if (order % 4 == 2 )
      StracheyMethod.solver
    else if (order % 4 == 0)
      DoublyEvenMethod.solver
    else
      bfsolveCross
  }

  import MSquare._

  private def printText(result: (Vector[RCD], Weight, Weight), time: Quantity[Double]): Unit = {
    val numSpaces = s"${order*order}".length
    val dashes = (1 to numSpaces).map( _ => '-').mkString("")
    println(s"Magic Constant = ${magicConstant(order)}")
    val midHead = (for (_ <- 2 to order) yield s"+$dashes").mkString
    val header = s"+$dashes$midHead+"
    println(header)
    result._1.foreach { row =>
      val mid = (for (cell <- row) yield {
        val fcell = cell.toString.reverse.padTo(numSpaces, ' ').reverse
        s"|$fcell"
      }).mkString
      println(s"$mid|")
    }
    println(header)
    println(s"Exec info: ${result._2} attemps of ${result._3} total attemps on $time")
  }

  var result: (Vector[RCD], Weight, Weight) = (Vector(), 0, 0)


  val time = measure {
    result = getSolverByMethod(order)
  }

  assert(verifyMSquare(order, result._1,  magicConstant(order)))

  printText(result, time)
  rotations(result._1).foreach(s => printText((s, 0, 0), time))
  reflections(result._1).foreach(s => printText((s, 0, 0), time))
}
