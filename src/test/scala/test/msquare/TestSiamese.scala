package test.msquare

import org.msquare.SiameseMethod
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TestSiamese extends AnyFlatSpec with Matchers {

  "Siamese" should "return the Square" in {
    val result = SiameseMethod.solver(1)(5)
    result._1.length should be(5)
  }
}
