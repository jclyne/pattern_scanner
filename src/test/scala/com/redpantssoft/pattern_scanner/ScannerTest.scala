package com.redpantssoft.pattern_scanner

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FunSpec
import io.Source.fromFile
import Preamble._
import io.{Source, Codec}
import java.io.File


@RunWith(classOf[JUnitRunner])
class ScannerTest extends FunSpec with ShouldMatchers {

  val ssn = Pattern(
    1.1,
    "ssn",
    """[[:digit:]]{3}[ \-][[:digit:]]{2}[ \-][[:digit:]]{4}"""
  )

  val ssn_unformated = Pattern(
    3.1,
    "ssn_unformatted",
    """[[:digit:]]{9}"""
  )


  val visa = Pattern(
    2.1,
    "visa",
    """4[[:digit:]]{3}([ \-]?[[:digit:]]{4}){3}"""
  )

  val digit = Pattern(
    4.1,
    "digit",
    """1[^13]"""
  )

  def patternMatchStringTest(name: String,
                             input: String,
                             expected: List[Scanner.Match],
                             ctxt: ScannerCtxt) {

    it( """should find pattern(s) """ + name + """ in a string""") {
      val scanner = Scanner(ctxt)
      val result = scanner.update(input) ::: scanner.complete
      result.length should equal(expected.length)
      result should equal(expected)
    }
  }

  def patternMatchFileTest(name: String,
                           input: Source,
                           expected: List[Scanner.Match],
                           ctxt: ScannerCtxt) {

    it( """should find pattern(s) """ + name + """ in a file""") {
      val scanner = Scanner(ctxt)
      val result = input.flatMap(scanner.update(_)).toList ::: scanner.complete
      result.length should equal(expected.length)
      result should equal(expected)
    }
  }

  describe("Scanner") {
    patternMatchStringTest(
      "digit",
      "12 ",
      Scanner.Match(4.1, "digit", 0, "12") :: Nil,
      ScannerCtxt(digit :: Nil)
    )

    patternMatchStringTest(
      "SSN",
      "Hi, here is my social security number 444-42-1234",
      Scanner.Match(1.1, "ssn", 38, "444-42-1234") :: Nil,
      ScannerCtxt(ssn :: visa :: Nil)
    )

    patternMatchStringTest(
      "VISA",
      "Hi, here is my visa number 4045124442700008, don't give it to anyone",
      Scanner.Match(2.1, "visa", 27, "4045124442700008") :: Nil,
      ScannerCtxt(ssn :: visa :: Nil)
    )

    patternMatchStringTest(
      "VISA Formatted",
      "Hi, here is my visa number 4045 1244 4270 0008, don't give it to anyone",
      Scanner.Match(2.1, "visa", 27, "4045 1244 4270 0008") :: Nil,
      ScannerCtxt(ssn :: visa :: Nil)
    )

    patternMatchStringTest(
      "VISA w/SSN unformatted",
      "Hi, here is my visa number 4045124442700008, don't give it to anyone",
      Scanner.Match(2.1, "visa", 27, "4045124442700008") :: Nil,
      ScannerCtxt(ssn :: ssn_unformated :: visa :: Nil)
    )

    patternMatchStringTest(
      "SSN unformatted",
      "Hi, here is my visa number 404512444270000, don't give it to anyone",
      Scanner.Match(3.1, "ssn_unformatted", 27, "404512444") :: Nil,
      ScannerCtxt(ssn :: ssn_unformated :: visa :: Nil)
    )

    patternMatchStringTest(
      "SSN, VISA",
      "Hi, here is my SSN is 444-42-1234 and  visa number is #4045124442700008, don't give it to anyone",
      Scanner.Match(1.1, "ssn", 22, "444-42-1234") ::
        Scanner.Match(2.1, "visa", 55, "4045124442700008") :: Nil,
      ScannerCtxt(ssn :: visa :: Nil)
    )

    patternMatchFileTest(
      "SSN, VISA",
      (fromFile(this.getClass.getResource("/has_ssn_visa.txt").getFile)(Codec.UTF8)),
      Scanner.Match(1.1, "ssn", 663014, "444-42-1234") ::
        Scanner.Match(2.1, "visa", 663047, "4045124442700008") :: Nil,
      ScannerCtxt(ssn :: visa :: Nil)
    )

  }
  describe("Scanner Serialization") {
    it("should serialize a scanner properly") {
      val name = "test_scanner"
      val ctxt = ScannerCtxt(ssn :: visa :: Nil)
      ctxt.serialize(name)
      val newScanner = Scanner(ScannerCtxt.deserialize(name))

      val input = "Hi, here is my social security number 444-42-1234"
      val expected = Scanner.Match(1.1, "ssn", 38, "444-42-1234") :: Nil
      val result = newScanner.update(input) ::: newScanner.complete
      result.length should equal(expected.length)
      result should equal(expected)
      (new File(name)).delete()

    }
  }

}