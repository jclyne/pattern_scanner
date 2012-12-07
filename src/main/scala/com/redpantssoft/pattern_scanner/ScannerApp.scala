package com.redpantssoft.pattern_scanner

import com.redpantssoft.logging.Logging
import io.Source
import io.Codec
import java.io.InvalidClassException
import com.redpantssoft.pattern_scanner.xml.PatternDefinitionXMLParser


/** Main function that allows for command line scanning
  *
  * This application method takes a pattern file and a
  * file to scan as arguments. It will compile a scanner
  * from the pattern file and scan the scan file, displaying
  * the matches
  *
  */
object ScannerApp extends App with Logging {

  val serializedName = "scanner_app"

  val scanFilePath = try {
    args(0)
  } catch {
    case e: IndexOutOfBoundsException =>
      error("Error: No scan file specified (arg 2)")
      sys.exit(1)
  }

  val input = try {
    Source.fromFile(scanFilePath)(Codec.UTF8)
  } catch {
    case e: Exception =>
      error(e.toString)
      sys.exit(1)
  }


  val ctxt = try {
    val parser = new PatternDefinitionXMLParser(this.getClass.getResource("/PatternDefinitionSchema.xsd").getFile)
    val ctxt = ScannerCtxt(parser.parse(args(1)))
    ctxt.serialize(serializedName)
    ctxt
  } catch {
    case e: Exception =>
      try {
        println("No pattern file specified, looking for context cache")
        ScannerCtxt.deserialize(serializedName)
      } catch {
        case e: InvalidClassException =>
          error("No valid pattern file or context cache found ")
          sys.exit(1)
      }
  }

  val scanner = Scanner(ctxt)

  println("Scanning file: " + scanFilePath)
  val result = input.flatMap(scanner.update(_)).toList
  if (result.length > 0) {
    println("Found " + result.length + " matches")
    result.foreach((m) => println(m + "\n"))
  }
  else
    println("No matches found")
}
