package com.redpantssoft.pattern_scanner.regexp

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import Preamble._

@RunWith(classOf[JUnitRunner])
class RegExpVectorTest extends FunSpec with ShouldMatchers {

  val vector = word :: blank :: cntrl :: print :: int :: hex :: Nil
  val testAlphabet = Range.inclusive(0x20, 0x7E).map(_.toChar)

  describe("RegExpVectorTest") {

    it("should produce consistent derivates") {
      for (c <- testAlphabet)
        vector.derive(c).vector.corresponds(vector)(_ == _.derive(c)) should be(true)
    }

    it("should produce a consistent derivation table") {
      val table = vector.derive()
      for (char <- testAlphabet)
        table.derive(char) should equal(vector.derive(char))
    }

    it("should have natural equality") {
      for (c <- testAlphabet) {
        val dr1 = new RegExpVector(vector).derive(c)
        val dr2 = new RegExpVector(vector.map(_.derive(c)))
        dr1 should equal(dr2)
        dr1 should not equal (RegExpVector(dr2.vector.reverse))
        RegExpVector(dr1.vector.reverse) should not equal (dr2)
      }
    }

    it("should have hash equality") {
      for (c <- testAlphabet) {
        val dr1 = new RegExpVector(vector).derive(c)
        val dr2 = new RegExpVector(vector.map(_.derive(c)))
        dr1.hashCode should equal(dr2.hashCode)
        dr1.hashCode should not equal (RegExpVector(dr2.vector.reverse).hashCode)
        RegExpVector(dr1.vector.reverse).hashCode should not equal (dr2.hashCode)
      }
    }
  }
}
