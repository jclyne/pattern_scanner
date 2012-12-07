package com.redpantssoft.pattern_scanner.regexp

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import Preamble._

@RunWith(classOf[JUnitRunner])
class RegExpCompilerTest extends FunSpec with ShouldMatchers {
  describe("RegExpCompilerTest") {

    it( """should compile a character literal properly""") {
      RegExp( """a""") should equal(Symbol('a'))
    }

    it( """should compile '.' properly""") {
      RegExp( """.""") should equal(any)
    }

    it( """should compile bracket set properly""") {
      RegExp( """[ab]""") should equal('a' || 'b')
      RegExp( """[abc]""") should equal('a' || 'b' || 'c')
    }

    it( """should compile negative bracket set properly""") {
      RegExp( """[^ab]""") should equal(And(Not(Or('a', 'b')), any))
      RegExp( """[^abc]""") should equal(And(Not(Or(Or('a', 'b'), 'c')), any))
    }

    it( """should compile bracket set range properly""") {
      RegExp( """[a-z]""") should equal('a' -- 'z')
    }

    it( """should compile bracket set intersection properly""") {
      RegExp( """[a-c]{-}[b-z]""") should equal('a' -- 'c' && !('b' -- 'z'))
    }

    it( """should compile a kleene star properly""") {
      RegExp( """a*""") should equal(Symbol('a').*)
    }

    it( """should compile the NOT properly""") {
      val r = RegExp( """1[^13]""")
      r.matches( """1""") should be(false)
      r.matches( """11""") should be(false)
      r.matches( """12""") should be(true)
      r.matches( """14""") should be(true)
      r.matches( """123""") should be(false)
    }

    it( """should compile the SSN pattern properly""") {
      val r = RegExp( """[^[:alnum:]\-][[:digit:]]{3}[ \-][[:digit:]]{2}[ \-][[:digit:]]{4}[^[:alnum:]\-]""")
      r.matches(" 444-42-1234 ") should be(true)
      r.matches(" 444-42-1240 ") should be(true)
      r.matches(" 446-60-3300") should be(false)
      r.matches(".444-42-1236 ") should be(true)
    }

    it( """should compile the VISA pattern properly""") {
      val r = RegExp( """[^[:alnum:]\-]4[[:digit:]]{3}([ \-]?[[:digit:]]{4}){3}[[:^alnum:]]""")
      r.matches( """4035805321379573""") should be(false)
      r.matches( """4035805321379573 """) should be(false)
      r.matches( """ 4035805321379573""") should be(false)
      r.matches( """ 4035805321379573"""") should be(true)
      r.matches( """ 4039801202217624&""") should be(true)
      r.matches( """ 4040392632239542'""") should be(true)
      r.matches( """ 4045124442700008,""") should be(true)
      r.matches( """ 4501-1192-7667-0456 """) should be(true)
      r.matches( """ 4501 1192 7667 0456 """) should be(true)
      r.matches( """ 4501 1192 7667 0456 """) should be(true)
      r.matches( """(4011931118120422 """) should be(true)
      r.matches( """;4020746055220707 """) should be(true)
      r.matches( """=4022293356329638 """) should be(true)
      r.matches( """>4023439536575249 """) should be(true)
      r.matches( """[4026867073028460 """) should be(true)
      r.matches( """|4031084342899019 """) should be(true)
    }
  }
}
