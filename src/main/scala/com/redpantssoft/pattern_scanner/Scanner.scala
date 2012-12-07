package com.redpantssoft.pattern_scanner

import automaton.State
import collection.mutable.ListBuffer
import com.redpantssoft.logging.Logging
import annotation.tailrec
import scala.util.control.Breaks._

/** Implements a Text scanner based on a sequence of patterns
  *
  * A scanner is scans for a sequence of Patterns and builds a
  * Finite State Automata capable of searching text with minimal
  * runtime overhead and O(1) complexity with regard to the number
  * of patterns
  *
  * When the scanner is run, it looks for strings that match any of
  * the input patterns. If it finds more than one match, it takes the
  * one matching the longest string text. If it finds two or more matches
  * of the same length, the rule listed first in the input is chosen.
  *
  * The matching is greedy, meaning the longest match will always be
  * returned. While scanning, all states representing a match are placed
  * in the match buffer. When a non-matching  state is reached, the scanner
  * will backtrack through the match buffer, from longest match to shortest.
  * The longest match is then returned.
  *
  * A scanner is instance constructed with a  ScannerCtxt object. A ScannerCtxt
  * is built with the ScannerCtxt compiler, which can compile a context from
  * numerous source types(com.redpantssoft.pattern_scanner.xml,json, binary). In any case, the compilation
  * step takes a long time and isn't needed unless there is a change to the
  * pattern list. In normal operation, a context should be compiled and new
  * Scanner instances created with the existing context until the context needs
  * to be changed.
  *
  * @param ctxt context for the scanner, represents what is being scanned for
  */
class Scanner private[pattern_scanner](ctxt: ScannerCtxt) extends Logging {

  import com.redpantssoft.pattern_scanner.Scanner._

  /** Buffer containing the current input text being scanned */
  private val scanBuffer = ListBuffer[Char]()

  /**
   * Defines an accepting state with the match string
   * @param state accepting state
   * @param matchStr matching string
   */
  case class AcceptingState(state: State[Char], matchStr: String)

  /** Buffer containing the current list of matches in the scanBuffer */
  private val matchBuffer = ListBuffer[AcceptingState]()

  /** Current positional offset in the input text */
  private var pos = 0

  /** Variable holding the current state of the state machine */
  private var currentState = ctxt.automaton.getStartState

  /**
   * Resets the scanner
   *
   * This will reset the pattern state machine and flush all buffers.
   * The scanner will be in a state identical to its state just after
   * construction.
   */
  def reset() {
    currentState = ctxt.automaton.getStartState
    matchBuffer.clear()
    scanBuffer.clear()
    pos = 0
  }

  /**
   * Advances the scanner by a string of characters optionally
   * returning a list of matches
   *
   * @param s string input
   * @return optional list of Scanner matches
   */
  def update(s: String): List[Scanner.Match] = {
    s.toList.flatMap(update(_))
  }

  /**
   * Advances the scanner a single character optionally
   * returning a list of matches
   *
   * @param c  character input
   * @return optional list of Scanner matches
   */
  def update(c: Char): List[Scanner.Match] = {
    trace("Scanner update '" + c + "'")

    scanBuffer += c
    val state = ctxt.automaton.transition(currentState, c)
    currentState = state
    checkForMatch(state) match {

      case Some(m: IgnoreMatch) =>
        advanceScanBuffer(m.string.length)
        processScanBuffer(complete = false)

      case Some(m) =>
        advanceScanBuffer(m.string.length)
        m :: processScanBuffer(complete = false)

      case None =>
        if (state.isFinal) {
          advanceScanBuffer(1)
          processScanBuffer(complete = false)
        } else
          Nil
    }
  }

  /**
   * Finalizes the current scanner
   *
   * This will finalize the scanner and return a list of any
   * remaining pattern matches that have yet to be returned.
   *
   * @return list of selected scanner matches not yet returned
   */
  def complete: List[Scanner.Match] = {
    trace("Scanner complete")

    if (scanBuffer.nonEmpty) {
      if (matchBuffer.nonEmpty) {
        backtrackMatchBuffer() match {

          case Some(m: IgnoreMatch) =>
            advanceScanBuffer(m.string.length)
            processScanBuffer(complete = true)

          case Some(m) =>
            advanceScanBuffer(m.string.length)
            m :: processScanBuffer(complete = true)

          case None =>
            advanceScanBuffer(1)
            processScanBuffer(complete = true)
        }
      }
      else {
        advanceScanBuffer(1)
        processScanBuffer(complete = true)
      }
    } else
      Nil
  }

  /**
   * Checks whether the current state indicates a match
   *
   * This will first check the current state for accepting. If it is
   * the AcceptingState is added to the matchBuffer. Once the current state
   * is final and the matchBuffer is not empty, it will backtrack through
   * the current matchBuffer looking for actual matches.
   *
   * @param state current state
   * @return optional scanner match from the match buffer
   */
  private def checkForMatch(state: State[Char], offset: Int = 0, length: Int = scanBuffer.length): Option[Scanner.Match] = {
    if (state.isAccepting) {
      val potentialMatch = AcceptingState(state, scanBuffer.slice(offset, length).mkString)
      debug("Pattern -> Potential Match='" + potentialMatch.matchStr + "'")
      potentialMatch +=: matchBuffer
    }

    if (matchBuffer.nonEmpty && state.isFinal)
      backtrackMatchBuffer()
    else
      None
  }

  /**
   * Processes the existing scan buffer
   * Once an accepting or final state is reached, the scan buffer
   * is advanced (from the left) and each sub string is scanned until
   * the longest remaining string with a normal state is reached.
   * This can result in numerous matches.
   *
   * @return list of matches as a result of processing the existing
   *         scan buffer
   */
  private def processScanBuffer(complete: Boolean = false): List[Scanner.Match] = {
    val result = ListBuffer[Scanner.Match]()
    while (scanBuffer.nonEmpty) {
      breakable {
        var length = 0
        for (c <- scanBuffer) {
          val state = ctxt.automaton.transition(currentState, c)
          currentState = state
          length = length + 1
          checkForMatch(state, length = length) match {
            case Some(_match) =>
              result += _match
              advanceScanBuffer(_match.string.length)
              break()
            case None =>
              if (state.isFinal) {
                advanceScanBuffer(1)
                break()
              }
          }
        }
        if (complete)
          backtrackMatchBuffer() match {
            case Some(_match) =>
              result += _match
              advanceScanBuffer(_match.string.length)
            case None =>
              advanceScanBuffer(1)
          }

        return result.toList
      }
    }

    result.toList
  }

  /**
   * Advances the scan buffer from the left
   * When a final or accepting state is reached, the string
   * that was being scanned is no longer needed. This advances the
   * buffer from the left trimming the characters as needed
   *
   * @param skip number of characters in the buffer to skip
   */
  private def advanceScanBuffer(skip: Int) {
    currentState = ctxt.automaton.getStartState
    matchBuffer.clear()
    scanBuffer.trimStart(skip)
    pos += skip
  }

  /**
   * Backtracks through the match buffer looking for the longest validated match
   * Each pattern has a validation predicate that will determine if the match
   * is accepted.
   *
   * @param mb current match buffer
   * @return  optional Scanner match
   */
  @tailrec
  private def backtrackMatchBuffer(mb: ListBuffer[AcceptingState] = matchBuffer): Option[Scanner.Match] = {
    if (mb.nonEmpty) {
      val matchStr = mb.head.matchStr
      ctxt.stateMap.get(mb.head.state.id) match {
        case Some(matched) =>
          matched match {
            case Nil => backtrackMatchBuffer(mb.tail)
            case p :: _ =>
              debug("Pattern -> " + p)
              val _match =
                if (p.ignore)
                  new Scanner.Match(p.id, p.name, pos, matchStr) with Scanner.IgnoreMatch
                else
                  Scanner.Match(p.id, p.name, pos, matchStr)
              debug("Pattern -> " + _match)
              Some(_match)
          }
        case None =>
          error("Invalid state map in pattern scanner, state " + mb.head.state.id + " is undefined")
          warn("Pattern Scanner is in invalid and should be recompiled")
          None
      }
    }
    else
      None
  }
}

object Scanner {

  def apply(ctxt: ScannerCtxt) = new Scanner(ctxt)

  /** Represents a Pattern Scanner Match
    *
    * @param id id of the matching pattern
    * @param name name of the matching pattern
    * @param pos offset into the input text where the match occurred
    * @param string  matching string
    */
  case class Match(id: Pattern.Id, name: String, pos: Long, string: String) {
    override def toString = "Match Name: " + name + "(" + id + ")" +
      " Offset: " + pos + " string: " + string
  }

  trait IgnoreMatch extends Match {
    override def toString = "Ignore Match Name: " +
      name + "(" + id + ")" + " Offset: " + pos + " string: " + string
  }

}


