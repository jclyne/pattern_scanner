package com.redpantssoft.pattern_scanner.automaton

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import com.redpantssoft.pattern_scanner.regexp.Preamble._
import com.redpantssoft.pattern_scanner.regexp._

@RunWith(classOf[JUnitRunner])
class DerivableAutomatonTest extends FunSpec with ShouldMatchers {

  describe("DerivableAutomatonTest") {

    def automatonTest[T](name: String, dr: Derivable[T], pat: String) {
      it("should generate a automaton equivalent to derivative map for " + name + ":" + pat) {
        DerivableAutomaton.matches(DerivableAutomaton.compile(dr), pat) should equal(dr.matches(pat))
      }
    }

    automatonTest("digits", digits, "0")
    automatonTest("digits", digits, "01")
    automatonTest("digits", digits, "56")
    automatonTest("digits", digits, "9999")
    automatonTest("digits", digits, "12341237867")
    automatonTest("digits", digits, "a")
    automatonTest("digits", digits, "3278a23423")
    automatonTest("hex", hex, "-0x01A4f")
    automatonTest("hex", hex, "+0x01A4f")
    automatonTest("hex", hex, "0x01A4f")
    automatonTest("hex", hex, "01A4f")
    automatonTest("hex", hex, "x01A4f")
    automatonTest("digit.*(3)", digit.*(3), "012")
    automatonTest("digit.*(2)", digit.*(2), "01")
    automatonTest("digit.*(1)", digit.*(1), "5")
    automatonTest("digit.*(4)", digit.*(4), "9999")
    automatonTest("digit.*(11)", digit.*(11), "12341237867")
    automatonTest("digit.*(5)", digit.*(5), "1")
    automatonTest("digit.*(1)", digit.*(1), "a")
    automatonTest("digit.*(1)", digit.*(1), "12")
    automatonTest("digit.*(3, 5)", digit.*(3, 5), "012")
    automatonTest("digit.*(3, 5)", digit.*(3, 5), "0123")
    automatonTest("digit.*(3, 5)", digit.*(3, 5), "01234")
    automatonTest("digit.*(3, 5)", digit.*(3, 5), "01")
    automatonTest("digit.*(3, 5)", digit.*(3, 5), "0")
    automatonTest("digit.*(3, 5)", digit.*(3, 5), "012345")
    automatonTest("digit.*(3, 5)", digit.*(3, 5), "0123456")
    automatonTest("digit.*(3, 3)", digit.*(3, 3), "012")
    automatonTest("digit.*(3, 3)", digit.*(3, 3), "01")
    automatonTest("digit.*(3, 3)", digit.*(3, 3), "0123")
    automatonTest("digit.*(3, 4)", digit.*(3, 4), "012")
    automatonTest("digit.*(3, 4)", digit.*(3, 4), "0123")
    automatonTest("digit.*(3, 4)", digit.*(3, 4), "01")
    automatonTest("digit.*(3, 4)", digit.*(3, 4), "01234")


    val p: RegExp = ("h" || "c").+ ++ "at"
    val pname = """("h" || "c").+ ++ "at""""
    automatonTest(pname, p, "hat")
    automatonTest(pname, p, "cat")
    automatonTest(pname, p, "hhat")
    automatonTest(pname, p, "chat")
    automatonTest(pname, p, "hcat")
    automatonTest(pname, p, "at")

    val p1: RegExp = ("h" || "c").? ++ "at"
    val p1name = """("h" || "c").? ++ "at""""
    automatonTest(p1name, p1, "hat")
    automatonTest(p1name, p1, "cat")
    automatonTest(p1name, p1, "at")
    automatonTest(p1name, p1, "chat")
    automatonTest(p1name, p1, "hcat")

    val p2: RegExp = ('h' ++ 'c').? ++ "at"
    val p2name = """('h' ++ 'c').? ++ "at""""
    automatonTest(p2name, p2, "hcat")
    automatonTest(p2name, p2, "at")
    automatonTest(p2name, p2, "hat")
    automatonTest(p2name, p2, "cat")

    val p3: RegExp = Cat(Symbol('1'), And(Not(Symbol('1')), any))
    val p3name = """1 ++ (!1 && any)"""
    automatonTest(p3name, p3, "1")
    automatonTest(p3name, p3, "11")
    automatonTest(p3name, p3, "12")
    automatonTest(p3name, p3, "123")

    val v = word :: blank :: cntrl :: print :: int :: hex :: Nil
    val vname = """word :: blank :: cntrl :: print :: int :: hex :: Nil"""
    automatonTest(vname, v, "-0x01A4f")
    automatonTest(vname, v, "+0x01A4f")
    automatonTest(vname, v, "0x01A4f")
    automatonTest(vname, v, "01A4f")
    automatonTest(vname, v, "x01A4f")
  }

}
