package com.redpantssoft.pattern_scanner.xml

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.{OneInstancePerTest, FunSpec}


@RunWith(classOf[JUnitRunner])
class PatternXmlTest extends FunSpec with OneInstancePerTest with ShouldMatchers {

  val patternSchemaPath = this.getClass.getResource("/PatternDefinitionSchema.xsd").getFile
  val patternFilePath = this.getClass.getResource("/patterns.xml").getFile

  describe("PatternXmlTest") {
    it("should parse the pattern_scanner.xml schema properly") {
      val parser = new PatternDefinitionXMLParser(patternSchemaPath)
      val patterns = parser.parse(patternFilePath)
      patterns.size should be > 0
    }
  }
}
