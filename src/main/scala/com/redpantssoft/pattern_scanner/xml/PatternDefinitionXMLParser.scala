package com.redpantssoft.pattern_scanner.xml

import javax.xml.validation.SchemaFactory
import javax.xml.XMLConstants
import javax.xml.transform.stream.StreamSource
import javax.xml.parsers.SAXParserFactory
import xml.factory.XMLLoader
import xml.Elem
import xml.parsing.NoBindingFactoryAdapter
import org.xml.sax.SAXParseException
import com.redpantssoft.logging.Logging
import com.redpantssoft.pattern_scanner.Pattern
import collection.mutable.ListBuffer
import java.io.{FileInputStream, IOException}

/** Implements an Parser for Pattern Definition Files
  * This will compile an XMLLoader with a parser that will validate
  * the XML according to the schema defined in PatternDefinitionSchema.xsd
  * It will also install an Error handler that will log errors and throw
  * a SAXParserException on any error.
  *
  * The apply method will parse the com.redpantssoft.pattern_scanner.xml file and return a list of patterns
  * as defined in the file
  */
class PatternDefinitionXMLParser(schemaFile: String) extends Logging {
  private val schemaFactory = SchemaFactory.newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI)

  /** Schema built from the PatternDefinitionSchema.xsd file */
  private val schema = schemaFactory.newSchema(
    new StreamSource(new FileInputStream(schemaFile))
  )

  /** SAX Parser factory that will validate against the pattern definition
    * schema */
  private val patternParserFactory = SAXParserFactory.newInstance()
  patternParserFactory.setValidating(false)
  patternParserFactory.setNamespaceAware(true)
  //saxParserFactory.setFeature("http://com.redpantssoft.pattern_scanner.xml.org/sax/features/namespace-prefixes", true)
  patternParserFactory.setSchema(schema)

  /** XMLLoader that uses the validating patternParserFactory and
    * handles all error */
  val loader = new XMLLoader[Elem] {
    override val parser = patternParserFactory.newSAXParser()
    override val adapter = new NoBindingFactoryAdapter {

      override def warning(e: SAXParseException) {
        throw new PatternDefinitionXMLParseException(e)
      }

      override def error(e: SAXParseException) {
        throw new PatternDefinitionXMLParseException(e)
      }

      override def fatalError(e: SAXParseException) {
        throw new PatternDefinitionXMLParseException(e)
      }
    }
  }

  /** Parses the pattern definition file xmlFilePath and returns a list of patterns
    * as defined in the file
    *
    * @param xmlFilePath path of XML file containing pattern definitions
    * @throws java.io.IOException thrown if there is an error with the file at xmlFilePath
    * @throws com.redpantssoft.pattern_scanner.xml.PatternDefinitionXMLParser.PatternDefinitionXMLParseException
   * thrown when there is a format error in the pattern file at xmlFilePath
    * @return
    */
  @throws(classOf[IOException])
  @throws(classOf[PatternDefinitionXMLParseException])
  def parse(xmlFilePath: String): List[Pattern] = {
    val ret = ListBuffer[Pattern]()
    loader.loadFile(xmlFilePath) match {
      // Find the enclosing Patterns tag
      case <Patterns>{patterns@_*}</Patterns> =>
        // Find each Pattern tag
        for (pattern@ <Pattern>{_*}</Pattern> <- patterns) {
          var idx = 0
          // Skip disabled patterns
          if ((pattern \ "Disabled").text.toLowerCase == "false") {
            // Find each defined regex in the pattern
            for (regex <- (pattern \ "RegEx")) {
              // look for patterns with boundaries
              if ((pattern \ "boundary").nonEmpty) {
                // if there is a boundary, build a unique pattern for each
                //  prefix->regex->suffix combination with an incrementing
                //  minor id
                for (boundary <- (pattern \ "boundary")) {
                  ret += Pattern(
                    id = Pattern.Id((pattern \ "Id").text.toInt, idx),
                    name = (pattern \ "Name").text,
                    regExp = (boundary \ "prefix").text + regex.text + (boundary \ "suffix").text,
                    ignore = (pattern \ "Ignore").text.toBoolean
                  )
                  idx += 1
                }
              } else {
                // No boundaries, just create single pattern for the regex
                ret += Pattern(
                  id = Pattern.Id((pattern \ "Id").text.toInt, idx),
                  name = (pattern \ "Name").text,
                  regExp = regex.text,
                  ignore = (pattern \ "Ignore").text.toBoolean
                )
                idx += 1
              }
            }
          }
        }
    }
    ret.toList
  }

}

/** Exception that occurs when attempting to parse an invalid pattern definition file
  *
  * @param message specified detailed message
  * @param e SAXParseException that thrown from the error
  */
class PatternDefinitionXMLParseException(message: String, e: SAXParseException)
  extends Exception(message, e) {


  /** Constructs a new exception with the specified SAXParseException */
  def this(e: SAXParseException) = this("", e)

  override def toString =
    if (message.nonEmpty) message + ": "
    else "" +
      "Error parsing pattern definition" +
      " at Line:" + e.getLineNumber + ", Column:" + e.getColumnNumber +
      ": " + e.getMessage
}



