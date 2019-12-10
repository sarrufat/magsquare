package org.msquare

import org.rogach.scallop.{ScallopConf, ScallopOption}
import org.scalameter._


class Conf(arguments: Seq[String]) extends ScallopConf(arguments) {
  val order: ScallopOption[Weight] = opt[Int](required = true, name = "order")
  val optimized = opt[Boolean](default=Some(false), name = "optimized")
  verify()
}

object MagicSquare extends App {
  val conf = new Conf(args)

  val order =  conf.order.toOption.get
  import MSquare._

  private def printText(result: (List[RCD], Weight, Weight),  time: Quantity[Double]) = {
    val midHead = (for( _ <- 2 to order) yield "+--").mkString
    val header = s"+--$midHead+"
    println(header)
    result._1.foreach{ row =>
      val mid = (for (cell <- row) yield {
        val fcell = cell.toString.reverse.padTo(2,' ').reverse
        s"|$fcell"
      }).mkString
      println(s"$mid|")
    }
    println(header)
    println(s"Exec info: ${result._2} attemps of ${result._3} total attemps on $time")
  }
  var result: (List[RCD], Weight, Weight) = (List(), 0, 0)
  val time = measure {
    conf.optimized.toOption match {
      case Some(true) =>
        result = bfsolveCross(order)
      case _ =>
        result = bfsolve(order)
    }
  }

  printText(result, time)
}
