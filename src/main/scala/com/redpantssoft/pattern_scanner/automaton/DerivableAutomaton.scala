package com.redpantssoft.pattern_scanner.automaton

import com.redpantssoft.pattern_scanner.regexp.Derivable
import collection.mutable.ArrayBuffer
import collection.mutable


/** Defines a state machine compiler from a Derivable
  *
  * This class is constructed via the factory method in the companion object.
  * It computes the derivatives of the Derivable and builds a finite state
  * machine to model the derivatives and character sets as states and
  * state transitions.
  *
  *
  */
object DerivableAutomaton {

  /** Factory method for DerivableAutomaton
    *
    * Uses a DerivableAutomaton.Compiler to construct a new
    * DerivableAutomaton from the specified Derivable
    *
    * @param derivable derivable to compile state machine
    * @tparam T type of derivable used to compile state machine
    * @return newly constructed DerivableAutomaton
    */
  def compile[T](derivable: Derivable[T],
                 stateNotify: Option[(State[Char], Derivable[T]) => Unit] = None) = stateNotify match {
    case Some(notify) => new Compiler(derivable, notify).automaton
    case None => new Compiler(derivable).automaton
  }


  /** Computes whether the string matches the pattern represented by the
    * state machine r
    *
    * @param s string to match against
    * @param a state machine to match with
    * @return indication of matching
    */
  def matches(a: Automaton[Char], s: String): Boolean = {
    def findMatch(s: String, r: State[Char]): Boolean = {
      if (s.isEmpty)
        r.isAccepting
      else
        findMatch(s.tail, r.input(s.head))
    }
    findMatch(s, a.start)
  }

  /** Implements a Compiler for  DerivableAutomaton from a specified Derivable
    *
    * Once an instance is created, the public member automaton contains the
    * newly constructed DerivableAutomaton.
    *
    * Its two construction options allow for optionally specifying a function
    * to be used to notify the client when a state has been created. This is
    * useful if there is a need to keep track of what the states in the
    * state machine represent. The notification function will be called
    * with the new state and the unique derivative that necessitated the state
    * creation.
    *
    * @param dr derivable to compile state machine representation of
    * @param stateNotify function used to notify that a state has been created
    * @tparam T  type of Derivable specified
    */
  class Compiler[T](dr: Derivable[T], stateNotify: (State[Char], Derivable[T]) => Unit) {

    def this(dr: Derivable[T]) = this(dr, (_, _) => Unit)

    private implicit def T2DerivableT(a: T): Derivable[T] = a.asInstanceOf[Derivable[T]]

    /** Mutable Table that contains the current list of derivatives and the
      * corresponding StateBuilder that represents the derivative. */
    private val stateTable = ArrayBuffer[(Derivable[T], State[Char])]()

    /** Mutable map that maps derivatives to state ID's, which correspond to their
      * index in the stateTable */
    private val stateMap = mutable.Map[Derivable[T], Int]()

    /** Automaton compilation statistics */
    private var _numStates = 0
    private var _numAcceptingStates = 0
    private var _numFinalStates = 0

    def numStates = _numStates

    def numAcceptingStates = _numAcceptingStates

    def numFinalStates = _numFinalStates


    /** Creates a new state for the specified derivative
      *
      * This will create a new state and store it in the stateTable referencing
      * the derivative
      *
      * @param dr derivable to compile a corresponding state
      * @return newly created state
      */
    private def createNewState(dr: Derivable[T]): State[Char] = {
      val (accepting, isFinal) = (dr.nullable, dr.isFinal)

      _numStates += 1
      if (accepting) _numAcceptingStates += 1
      if (isFinal) _numFinalStates += 1

      val newState = new AbstractState[Char](
        id = stateTable.length,
        isAccepting = accepting,
        isFinal = isFinal)
      stateNotify(newState, dr)
      stateTable += ((dr, newState))
      stateMap += ((dr, stateTable.length - 1))
      newState
    }

    /** Convenience method that will create a new state
      * if an existing state is not found
      *
      * @param dr derivable to find corresponding state or compile new one for
      * @return  existing or newly created state
      */
    private def findExistingOrCreateNewState(dr: Derivable[T]): State[Char] =
      stateMap.get(dr) match {
        case Some(idx: Int) => stateTable(idx)._2
        case None => createNewState(dr)
      }

    /** Compiles the finite state machine from the specified derivable
      *
      * The algorithm looks like this:
      * 1. Compute start state from the initial derivable, make it the
      * current state,and call it State #1
      * 2. Compute the DerivationMap for the current state.
      * 3. For each new derivative, lookup an existing state.
      * 4. For each existing state, add each input the as a entry in
      * the next map.
      * 5. For each non-existing state, create a new state and update inputs
      * 6. continue until every state in the stateTable has been processed
      *
      * @param dr derivative to compile state machine from
      * @return initial state for a newly created finite state machine
      */
    private def compile(dr: Derivable[T]): Automaton[Char] = {
      var idx = 0
      val startState = createNewState(dr)
      while (stateTable.isDefinedAt(idx)) {
        val (curDr, curState) = stateTable(idx)
        val drv = curDr.derive()
        curState.asInstanceOf[AbstractState[Char]].next = (for {(dr, chars) <- drv.list
                                                                state = findExistingOrCreateNewState(dr)
                                                                char <- chars
        } yield (char, state)).toMap
        curState.asInstanceOf[AbstractState[Char]].default = findExistingOrCreateNewState(drv.default)
        idx += 1
      }

      new Automaton(startState)
    }

    /** Newly constructed  DerivableAutomaton */
    val automaton = compile(dr)

  }

}


