package com.redpantssoft.pattern_scanner.regexp

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import Preamble._

@RunWith(classOf[JUnitRunner])
class RegExpTest extends FunSpec with ShouldMatchers {

  describe("RegExpTest") {

    it("should match . to any Symbol") {
      any matches "a" should be(true)
      any matches "1" should be(true)
      any matches ")" should be(true)
      any matches "ab" should be(false)

      val p: RegExp = 'a' ++ any ++ 'c'
      p matches "abc" should be(true)
      p matches "a1c" should be(true)
      p matches "azc" should be(true)
      p matches "a)c" should be(true)
      p matches "abbc" should be(false)

    }

    it("should match literal") {
      "a" matches "a" should be(true)
      'a' matches "a" should be(true)
      "a" matches "b" should be(false)
      "b" matches "a" should be(false)
    }

    it("should not match negated literal") {
      !"a" matches "a" should be(false)
      !Symbol('a') matches "a" should be(false)
      !"ab" matches "ab" should be(false)

      !"a" matches "ab" should be(true)
      !"a" matches "b" should be(true)
      !"b" matches "a" should be(true)
      !"ab" matches "a" should be(true)
      !"ab" matches "b" should be(true)
      !"ab" matches "ba" should be(true)

    }

    it("should match multiple literals") {
      "ab" matches "ab" should be(true)
      "abc" matches "abc" should be(true)
      ("ab".toRegExp ++ "c") matches "abc" should be(true)
      ("ab".toRegExp ++ "c") matches "abc" should be(true)
      ("a".toRegExp ++ "b" ++ "c") matches "abc" should be(true)
      "ab" matches "abc" should be(false)
      "ba" matches "ab" should be(false)
      "ad" matches "ba" should be(false)
      "abc" matches "ab" should be(false)
      "abc" matches "abd" should be(false)
      "abc" matches "dbc" should be(false)
      "aBc" matches "dbc" should be(false)
    }

    it("should match ranges of literals") {
      digit matches "0" should be(true)
      digit matches "1" should be(true)
      digit matches "2" should be(true)
      digit matches "a" should be(false)
      digit matches "12" should be(false)
    }

    it("should only allow ranges of literals") {
      intercept[IllegalArgumentException](("a" || "b") -- "c")
      intercept[IllegalArgumentException]("a" -- ("b" || "c"))
    }

    it("should disallow invalid ranges of literals") {
      intercept[IllegalArgumentException]('z' -- 'a')
      intercept[IllegalArgumentException]('9' -- '1')
    }

    it("should match repeating expressions") {
      digits matches "0" should be(true)
      digits matches "01" should be(true)
      digits matches "56" should be(true)
      digits matches "9999" should be(true)
      digits matches "12341237867" should be(true)
      digits matches "a" should be(false)
      digits matches "3278a23423" should be(false)

      hex matches "-0x01A4f" should be(true)
      hex matches "+0x01A4f" should be(true)
      hex matches "0x01A4f" should be(true)
      hex matches "01A4f" should be(true)
      hex matches "x01A4f" should be(false)
    }

    it("should match repeating expressions of a specific size") {
      digit.*(3) matches "012" should be(true)
      digit.*(2) matches "01" should be(true)
      digit.*(1) matches "5" should be(true)
      digit.*(4) matches "9999" should be(true)
      digit.*(11) matches "12341237867" should be(true)
      digit.*(5) matches "1" should be(false)
      digit.*(1) matches "a" should be(false)
      digit.*(1) matches "12" should be(false)
    }

    it("should match repeating expressions of a size min and max") {
      digit.*(3, 5) matches "012" should be(true)
      digit.*(3, 5) matches "0123" should be(true)
      digit.*(3, 5) matches "01234" should be(true)
      digit.*(3, 5) matches "01" should be(false)
      digit.*(3, 5) matches "0" should be(false)
      digit.*(3, 5) matches "012345" should be(false)
      digit.*(3, 5) matches "0123456" should be(false)
      digit.*(3, 3) matches "012" should be(true)
      digit.*(3, 3) matches "01" should be(false)
      digit.*(3, 3) matches "0123" should be(false)
      digit.*(3, 4) matches "012" should be(true)
      digit.*(3, 4) matches "0123" should be(true)
      digit.*(3, 4) matches "01" should be(false)
      digit.*(3, 4) matches "01234" should be(false)
    }

    it("should match one or more expressions") {
      val p: RegExp = ("h" || "c").+ ++ "at"
      p matches "hat" should be(true)
      p matches "cat" should be(true)
      p matches "hhat" should be(true)
      p matches "chat" should be(true)
      p matches "hcat" should be(true)
      p matches "at" should be(false)
    }

    it("should match zeroOrOne expressions") {
      val p1: RegExp = ("h" || "c").? ++ "at"
      p1 matches "hat" should be(true)
      p1 matches "cat" should be(true)
      p1 matches "at" should be(true)
      p1 matches "chat" should be(false)
      p1 matches "hcat" should be(false)

      val p2: RegExp = ('h' ++ 'c').? ++ "at"
      p2 matches "hcat" should be(true)
      p2 matches "at" should be(true)
      p2 matches "hat" should be(false)
      p2 matches "cat" should be(false)
    }

    it("should produce a consistent derivation table") {
      val testAlphabet = Range.inclusive(0x20, 0x7E).map(_.toChar)
      def testDerivation(p: RegExp) {
        val table = p.derive()
        for (char <- testAlphabet)
          table.derive(char) should equal(p.derive(char))
      }

      testDerivation(digit)
      testDerivation(alpha)
      testDerivation(int)
      testDerivation(hex)
    }

    it("should have natural equality") {
      emptySet should equal(emptySet)
      emptySet should not equal (emptyStr)
      emptySet should not equal (any)
      emptyStr should equal(emptyStr)
      emptyStr should not equal (emptySet)
      emptyStr should not equal (any)
      any should equal(any)
      any should not equal (emptyStr)
      any should not equal (emptySet)
      Symbol('a') should equal(Symbol('a'))
      Symbol('a') should not equal (Symbol('b'))
      Symbol('a') should not equal (any)
      Symbol('a') should not equal (emptyStr)
      Symbol('a') should not equal (emptySet)
      Star('a') should equal(Star('a'))
      Star('a') should not equal (Star('b'))
      'a' ++ 'b' should equal('a' ++ 'b')
      'a' ++ 'b' should not equal ('a' ++ 'c')
      'a' ++ 'b' should not equal ('c' ++ 'b')
      'a' ++ 'b' should not equal ('b' ++ 'a')
      Or('a', 'b') should equal(Or('a', 'b'))
      Or('a', 'b') should not equal (Or('a', 'c'))
    }
    it("should have hash equality") {
      emptySet.hashCode should equal(emptySet.hashCode)
      emptySet.hashCode should not equal (emptyStr.hashCode)
      emptySet.hashCode should not equal (any.hashCode)
      emptyStr.hashCode should equal(emptyStr.hashCode)
      emptyStr.hashCode should not equal (emptySet.hashCode)
      emptyStr.hashCode should not equal (any.hashCode)
      any.hashCode should equal(any.hashCode)
      any.hashCode should not equal (emptyStr.hashCode)
      any.hashCode should not equal (emptySet.hashCode)
      Symbol('a').hashCode should equal(Symbol('a').hashCode)
      Symbol('a').hashCode should not equal (Symbol('b').hashCode)
      Symbol('a').hashCode should not equal (any.hashCode)
      Symbol('a').hashCode should not equal (emptyStr.hashCode)
      Symbol('a').hashCode should not equal (emptySet.hashCode)
      Star('a').hashCode should equal(Star('a').hashCode)
      Star('a'.hashCode) should not equal (Star('b').hashCode)
      ('a' ++ 'b') should equal('a' ++ 'b')
      ('a' ++ 'b') should not equal (('a' ++ 'c').hashCode)
      ('a' ++ 'b') should not equal (('c' ++ 'b').hashCode)
      ('a' ++ 'b') should not equal (('b' ++ 'a').hashCode)
      Or('a', 'b').hashCode should equal(Or('a', 'b').hashCode)
      Or('a', 'b'.hashCode) should not equal (Or('a', 'c').hashCode)
    }

    def equivalenceTests(r: RegExp, s: RegExp, t: RegExp) {
      val suffix = " with r=" + r + ", s=" + s + ", t=" + t

      it("should have equivalence: (r || r == r)" + suffix) {
        (r || r) should equal(r)
        r should equal(r || r)
      }

      it("should have hash equivalence: (r || r == r)" + suffix) {
        (r || r).hashCode() should equal(r.hashCode)
        r.hashCode() should equal((r || r).hashCode)
      }

      it("should have equivalence: (r || s == s || r)" + suffix) {
        (r || s) should equal(s || r)
        (s || r) should equal(r || s)
      }

      it("should have hash equivalence: (r || s == s || r)" + suffix) {
        (r || s).hashCode should equal((s || r).hashCode)
        (s || r).hashCode should equal((r || s).hashCode)
      }

      it("should have equivalence: ((r || s) || t == r || (s || t))" + suffix) {
        ((r || s) || t) should equal((r || (s || t)))
        ((s || r) || t) should equal((r || (s || t)))
        ((r || s) || t) should equal((r || (t || s)))
        (r || (s || t)) should equal(((r || s) || t))
        (r || (t || s)) should equal(((r || s) || t))
        (r || (s || t)) should equal(((s || r) || t))
      }

      it("should have hash equivalence: ((r || s) || t == r || (s || t))" + suffix) {
        ((r || s) || t).hashCode should equal((r || (s || t)).hashCode)
        ((s || r) || t).hashCode should equal((r || (s || t)).hashCode)
        ((r || s) || t).hashCode should equal((r || (t || s)).hashCode)
        (r || (s || t)).hashCode should equal(((r || s) || t).hashCode)
        (r || (t || s)).hashCode should equal(((r || s) || t).hashCode)
        (r || (s || t)).hashCode should equal(((s || r) || t).hashCode)
      }

      it("should have equivalence: (!emptySet || r == !emptySet)" + suffix) {
        (!emptySet || r) should equal(!emptySet)
        (r || !emptySet) should equal(!emptySet)
        !emptySet should equal((!emptySet || r))
        !emptySet should equal((r || !emptySet))
      }

      it("should have hash equivalence: (!emptySet || r == !emptySet)" + suffix) {

        (!emptySet || r).hashCode should equal((!emptySet).hashCode)
        (r || !emptySet).hashCode should equal((!emptySet).hashCode)
        (!emptySet).hashCode should equal((!emptySet || r).hashCode)
        (!emptySet).hashCode should equal((r || !emptySet).hashCode)
      }

      it("should have equivalence: (emptySet || r == r)" + suffix) {
        (emptySet || r) should equal(r)
        (r || emptySet) should equal(r)
        r should equal((emptySet || r))
        r should equal((r || emptySet))
      }

      it("should have hash equivalence: (emptySet || r == r)" + suffix) {
        (emptySet || r).hashCode should equal(r.hashCode)
        (r || emptySet).hashCode should equal(r.hashCode)
        r.hashCode should equal((emptySet || r).hashCode)
        r.hashCode should equal((r || emptySet).hashCode)
      }

      it("should have equivalence: (r && r == r)" + suffix) {
        (r && r) should equal(r)
        r should equal((r && r))
      }

      it("should have hash equivalence: (r && r == r)" + suffix) {
        (r && r).hashCode should equal(r.hashCode)
        r.hashCode should equal((r && r).hashCode)
      }

      it("should have equivalence: (r && s == s && r)" + suffix) {
        (r && s) should equal((s && r))
        (s && r) should equal((r && s))
      }

      it("should have hash equivalence: (r && s == s && r)" + suffix) {
        (r && s).hashCode should equal((s && r).hashCode)
        (s && r).hashCode should equal((r && s).hashCode)
      }

      it("should have equivalence: ((r && s) && t == r && (s && t))" + suffix) {
        ((r && s) && t) should equal((r && (s && t)))
        ((s && r) && t) should equal((r && (s && t)))
        ((r && s) && t) should equal((r && (t && s)))
        (r && (s && t)) should equal(((r && s) && t))
        (r && (t && s)) should equal(((r && s) && t))
        (r && (s && t)) should equal(((s && r) && t))
      }

      it("should have hash equivalence: ((r && s) && t == r && (s && t))" + suffix) {
        ((r && s) && t).hashCode should equal((r && (s && t)).hashCode)
        ((s && r) && t).hashCode should equal((r && (s && t)).hashCode)
        ((r && s) && t).hashCode should equal((r && (t && s)).hashCode)
        (r && (s && t)).hashCode should equal(((r && s) && t).hashCode)
        (r && (t && s)).hashCode should equal(((r && s) && t).hashCode)
        (r && (s && t)).hashCode should equal(((s && r) && t).hashCode)
      }

      it("should have equivalence: (!emptySet && r == r)" + suffix) {
        (!emptySet && r) should equal(r)
        (r && !emptySet) should equal(r)
        r should equal((!emptySet && r))
        r should equal((r && !emptySet))
      }

      it("should have hash equivalence: (!emptySet && r == r)" + suffix) {
        (!emptySet && r).hashCode should equal(r.hashCode)
        (r && !emptySet).hashCode should equal(r.hashCode)
        r.hashCode should equal((!emptySet && r).hashCode)
        r.hashCode should equal((r && !emptySet).hashCode)
      }

      it("should have equivalence: (emptySet && r == emptySet)" + suffix) {
        (emptySet && r) should equal(emptySet)
        (r && emptySet) should equal(emptySet)
        emptySet should equal((emptySet && r))
        emptySet should equal((r && emptySet))
      }

      it("should have hash equivalence: (emptySet && r == emptySet)" + suffix) {
        (emptySet && r).hashCode should equal(emptySet.hashCode)
        (r && emptySet).hashCode should equal(emptySet.hashCode)
        emptySet.hashCode should equal((emptySet && r).hashCode)
        emptySet.hashCode should equal((r && emptySet).hashCode)
      }

      it("should have equivalence: ((r ++ s) ++ t == r ++ (s ++ t))" + suffix) {
        ((r ++ s) ++ t) should equal((r ++ (s ++ t)))
        (r ++ (s ++ t)) should equal(((r ++ s) ++ t))
      }

      it("should have hash equivalence: ((r ++ s) ++ t == r ++ (s ++ t))" + suffix) {
        ((r ++ s) ++ t).hashCode should equal((r ++ (s ++ t)).hashCode)
        (r ++ (s ++ t)).hashCode should equal(((r ++ s) ++ t).hashCode)
      }

      it("should have equivalence: (emptySet ++ r == emptySet)" + suffix) {
        (emptySet ++ r) should equal(emptySet)
        emptySet should equal((emptySet ++ r))
      }

      it("should have hash equivalence: (emptySet ++ r == emptySet)" + suffix) {
        (emptySet ++ r).hashCode should equal(emptySet.hashCode)
        emptySet.hashCode should equal((emptySet ++ r).hashCode)
      }

      it("should have equivalence: (r ++ emptySet == emptySet)" + suffix) {
        (r ++ emptySet) should equal(emptySet)
        emptySet should equal((r ++ emptySet))
      }

      it("should have hash equivalence: (r ++ emptySet == emptySet)" + suffix) {
        (r ++ emptySet).hashCode should equal(emptySet.hashCode)
        emptySet.hashCode should equal((r ++ emptySet).hashCode)
      }

      it("should have equivalence: (emptyStr ++ r == emptyStr)" + suffix) {
        (emptyStr ++ r) should equal(r)
        r should equal((emptyStr ++ r))
      }

      it("should have hash equivalence: (emptyStr ++ r == emptyStr)" + suffix) {
        (emptyStr ++ r).hashCode should equal(r.hashCode)
        r.hashCode should equal((emptyStr ++ r).hashCode)
      }

      it("should have equivalence: (r ++ emptyStr == emptyStr)" + suffix) {
        (r ++ emptyStr) should equal(r)
        r should equal((r ++ emptyStr))
      }

      it("should have hash equivalence: (r ++ emptyStr == emptyStr)" + suffix) {
        (r ++ emptyStr).hashCode should equal(r.hashCode)
        r.hashCode should equal((r ++ emptyStr).hashCode)
      }

      it("should have equivalence: ((r*)* == r*)" + suffix) {
        ((r *) *) should equal((r *))
        (r *) should equal(((r *) *))
      }

      it("should have hash  equivalence: ((r*)* == r*)" + suffix) {
        ((r *) *).hashCode should equal((r *).hashCode)
        (r *).hashCode should equal(((r *) *).hashCode)
      }

      it("should have equivalence (Not(Not(r) == r)" + suffix) {
        Not(Not(r)) should equal(r)
        r should equal(Not(Not(r)))
      }

      it("should have hash equivalence (Not(Not(r) == r)" + suffix) {
        Not(Not(r)).hashCode should equal(r.hashCode)
        r.hashCode should equal(Not(Not(r)).hashCode)
      }
    }

    it("should have equivalence: (emptyStr* == emptyStr") {
      (emptyStr *) == emptyStr should be(true)
      emptyStr == (emptyStr *) should be(true)
    }

    it("should have equivalence: (emptySet* == emptyStr") {
      (emptySet *) == emptyStr should be(true)
      emptyStr == (emptySet *) should be(true)
    }
    equivalenceTests(any, any, any)
    equivalenceTests(any, Symbol('b'), Symbol('c'))
    equivalenceTests(Symbol('a'), any, Symbol('c'))
    equivalenceTests(Symbol('a'), Symbol('b'), any)
    equivalenceTests(Symbol('a'), Symbol('a'), Symbol('a'))
    equivalenceTests(emptySet, emptySet, emptySet)
    equivalenceTests(emptyStr, emptyStr, emptyStr)
    equivalenceTests(emptyStr, emptySet, emptyStr)
    equivalenceTests(emptySet, emptyStr, emptyStr)
    equivalenceTests(Symbol('a'), Symbol('b'), Symbol('c'))
    equivalenceTests(Symbol('a') || Symbol('b'), Symbol('c') ++ Symbol('d'), Symbol('e'))
    equivalenceTests(Symbol('a') ++ Symbol('b'), Symbol('c') || Symbol('d'), Symbol('e'))
    equivalenceTests(Symbol('a') *, (Symbol('c') *) || (Symbol('d') *), Symbol('e') *)
    equivalenceTests(Symbol('a') ?, (Symbol('c') *) || (Symbol('d') *), Symbol('e') ?)
    equivalenceTests(Symbol('a') *, (Symbol('c') ?) || (Symbol('d') *), Symbol('e') *)
    equivalenceTests(hex, int, digits)
    equivalenceTests(int, hex, digits)
  }

}
