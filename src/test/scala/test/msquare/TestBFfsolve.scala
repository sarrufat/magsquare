package test.msquare

import org.msquare.MSquare
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TestBFfsolve extends AnyFlatSpec with Matchers {
  "bfsolve" should "return some Square" in {
    for( i <- 1 to 10000)
	    println(MSquare.bfsolveCross(4))
  }
}
