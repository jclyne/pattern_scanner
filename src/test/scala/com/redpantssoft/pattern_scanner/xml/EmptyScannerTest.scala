package com.redpantssoft.pattern_scanner.xml

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{OneInstancePerTest, FunSpec}
import org.scalatest.matchers.ShouldMatchers
import com.redpantssoft.pattern_scanner.{Scanner, ScannerCtxt}

@RunWith(classOf[JUnitRunner])
class EmptyScannerTest extends FunSpec with OneInstancePerTest with ShouldMatchers {

  describe("ScannerCtxt") {
    it("should build a valid empty context") {
      ScannerCtxt.empty()
    }
  }

  describe("Scanner") {
    it("should handle an empty context") {
      val emptyCtxt = ScannerCtxt.empty()
      val emptyScanner = Scanner(emptyCtxt)

      emptyScanner.update("Does This Match") should equal(Nil)
      emptyScanner.complete should equal(Nil)
    }
  }

}
