package com.redpantssoft.pattern_scanner

import automaton._
import com.redpantssoft.logging.Logging
import regexp.{RegExp, RegExpParserException, RegExpVector, Derivable}
import java.io._


/** Represents the compiled context of a pattern scanner
  *
  * A scanner requires a compiled scanner context for instantiation. A
  * scanner context does not need to be recompiled until there is a
  * need for change to the patterns being searched. In most cases,
  * these should be compiled only when needed and new Scanner instances
  * created with the existing context
  *
  * @param stateMap maps state id's to lists of patterns to associate
  *                 matching patterns to accepting states
  * @param automaton compiled finite state machine used to scan
  */
@SerialVersionUID(1L)
case class ScannerCtxt(stateMap: ScannerCtxt.StateMap, automaton: Automaton[Char])
  extends Logging {

  def serialize(name: String) {
    val fout = new FileOutputStream(name)
    try {
      val out = new ObjectOutputStream(fout)
      try {
        out.writeObject(this)
      } finally {
        out.close()
      }
    } catch {
      case e: Exception => error("Error during scanner ctxt '" + name + "'serialization: " + e)
    } finally {
      fout.close()
    }
  }
}

object ScannerCtxt extends Logging {

  type StateMap = Map[State.Id, List[Pattern]]

  /** Constructs a new ScannerCtxt, based on sequence of patterns
    * This uses the ScannerCtxt Builder to compile a new scanner instance
    *
    * @param patterns patterns the newly constructed scanner will scan for
    * @return newly instantiated scanner
    */
  def apply(patterns: Seq[Pattern]): ScannerCtxt = new Builder(patterns).ctxt

  /** Allows for deserialization of a binary representation of ScannerCtxt
    *
    * @param filePath path to file that contains the binary representation
    * @throws java.io.IOException thrown if there is an error with the file at filePath
    * @throws java.io.StreamCorruptedException thrown if the serialized stream is invalid
    * @throws java.lang.ClassNotFoundException  thrown if the class or version of the class in
    *                                           the serialized file does not match what is trying to be deserialized
    * @return
    */
  @throws(classOf[IOException])
  @throws(classOf[StreamCorruptedException])
  @throws(classOf[ClassNotFoundException])
  def deserialize(filePath: String): ScannerCtxt = {
    val cacheFileStream = new FileInputStream(filePath)
    try {
      // This makes sure to use the callers thread context class loader
      //  to deal with OSGI classloader issues
      val cacheObjectStream = new ObjectInputStream(cacheFileStream) {
        override def resolveClass(desc: ObjectStreamClass) = {
          val loader = this.getClass.getClassLoader
          loader.loadClass(desc.getName)
        }
      }
      try {
        cacheObjectStream.readObject().asInstanceOf[ScannerCtxt]
      } finally {
        cacheObjectStream.close()
      }
    } catch {
      case e: Exception =>
        error("Error during scanner context '" + filePath + "' deserialization: " + e)
        error(e.getStackTraceString)
        throw e
    } finally {
      cacheFileStream.close()
    }
  }


  /**
   * Builds an empty scanner ctxt
   *
   * @return empty scanner ctxt
   */
  def empty(): ScannerCtxt = {
    new Builder(Nil).ctxt
  }

  /** Implements the Pattern ScannerCtxt builder
    * This factory class compiles each regular expression from the
    * supplied sequence of patterns and then compiles an Automaton
    * based on a RegExpVector containing the compiled regular expressions.
    * A state map (maps states to matching patterns) is updated with callbacks
    * from the DerivableAutomaton compiler.
    *
    * @param patterns patterns the newly constructed scanner will scan for
    */
  private class Builder(patterns: Seq[Pattern]) extends Logging {

    info("Pattern scanner context compiler is compiling " + patterns.length + " patterns")

    val ctxt =
      if (patterns.length > 0) {

        var stateMap = Map[State.Id, List[Pattern]]()

        val patternArray = patterns.toArray
        /** Callback from Derivable Automaton Compiler that updates the
          * state map with states => accepting patterns
          */
        val addState: (State[Char], Derivable[RegExpVector]) => Unit = (state, dr) => {
          if (state.isAccepting)
            for ((dr, idx) <- dr.asInstanceOf[RegExpVector].vector.zipWithIndex)
              if (dr.nullable)
                stateMap = stateMap + ((state.id, patternArray(idx) :: stateMap.getOrElse(state.id, Nil)))
        }

        val RegExpVector = new RegExpVector(
          patterns.flatMap {
            case p: Pattern =>
              try {
                debug(p.toString)
                RegExp(p.regExp) :: Nil
              } catch {
                case e: RegExpParserException =>
                  error( """Error in pattern """" + p.id + """" - """ + e.getMessage)
                  Nil

              }
          }.toList)


        val start = System.currentTimeMillis()
        val compiler = new DerivableAutomaton.Compiler(RegExpVector, addState)
        val compilationTime = System.currentTimeMillis() - start
        info("Pattern scanner context state machine compilation completed in " + compilationTime + " ms")
        info("       Total States:" + compiler.numStates)
        info("   Accepting States:" + compiler.numAcceptingStates)
        info("       Final States:" + compiler.numFinalStates)
        info("       Total Size:" + compiler.automaton.start)

        new ScannerCtxt(stateMap, compiler.automaton)
      } else {
        new ScannerCtxt(Map(), new Automaton[Char](FinalState.asInstanceOf[State[Char]]))
      }
  }

}