package com.redpantssoft.pattern_scanner.automaton

import java.io.{FileOutputStream, OutputStreamWriter}


/** Represents a Finite-state machine
  *
  * The machine is in only one state at a time, represented by 'currentState'. It can
  * change from one state to another when initiated by a transition.  The transition
  * input is generic in type. Each state has a set of defined transitions as well as a
  * default transition.  When the machine is at the state 'start', it is considered reset.
  *
  * Any state in the machine can be accepting or final. An accepting state is one that
  * indicates that the machine has accepted the input thus far. A final state indicates that
  * there is no condition that will transition the state to a different state. An accepting
  * final state indicates that the machine will always accept any condition. Likewise, the
  * non-accepting, final state  indicates that no condition will transition to a new state.
  *
  * @param start - initial state
  * @tparam T  - type of the transition input
  */
@SerialVersionUID(1L)
class Automaton[T](private[pattern_scanner] val start: State[T]) extends Serializable {

  /** Sets a condition to transition the machine, based on the input condition
    *
    * @param currentState - current state
    * @param condition - state transition condition
    * @return current state of the automaton after inputting the condition
    */
  def transition(currentState: State[T], condition: T): State[T] = {
    currentState.input(condition)
  }

  def getStartState: State[T] = start


  /** Generates a DOT file for the constructed state machine.
    *
    * DOT is a plain text graph description language. It is a simple way of describing
    * graphs that both humans and computer programs can use. DOT graphs are typically
    * files that end with the .gv (or .any) extension.
    *
    * Various programs can process DOT files. Some, like any, neato, twopi, circo,
    * fdp, and sfdp, will read a DOT file and render it in graphical form. Others,
    * like gvpr, gc, accyclic, ccomps, sccmap, and tred, will read a DOT file and
    * perform calculations on the represented graph. Finally, others, like GVedit,
    * KGraphEditor, lefty, dotty, and grappa, provide an interactive interface. Most
    * programs are part of the Graphviz package or using it internally.
    *
    * @param path location of DOT file to generate
    */
  def generateDotFile(path: String) {

    // Verify file extension and open the output stream writer for the file
    val file = new OutputStreamWriter(
      new FileOutputStream(
        path + (if (path.endsWith(".gv") || path.endsWith(".dot")) "" else ".gv")
      ), "UTF-16"
    )

    var labeledStates = Set[State[T]]()

    def writeStateLabel(state: AbstractState[T]) {
      // Write out each state with the id string as the label
      file.write("  " + state.id + "[label=\"" + state.id + "\"")
      // Accepting states are boxes while others are elliptical
      if (state.isAccepting)
        file.write(" shape=box")
      file.write("];\n")
    }

    def writeStateTransitions(state: AbstractState[T]) {
      // Walk through each next state and draw the transitions
      //  Each transition is labeled with the set of conditions
      state.next.groupBy(_._2).foreach {
        case (nextState, states) =>
          file.write("  " + state.id + " -> " + nextState.id + " [label=\"")
          val conditions = states.keys.map {
            cond: T => cond match {
              case c: Char if c == '"' => "\\\""
              case c: Char if c == '\n' => "\\n"
              case c: Char if c == '\\' => "\\ "
              case _ => cond.toString
            }
          }.toArray.sorted
          file.write((conditions.head /: conditions.tail)(_ + "," + _))
          file.write("\"]" + ";\n")
      }
      // Write out the default state transition
      file.write("  " + state.id + " -> " + state.default.id + " [label=\"default\"];\n")
    }

    def writeState(state: AbstractState[T]) {
      if (labeledStates contains state) return

      writeStateLabel(state)
      writeStateTransitions(state)
      labeledStates = labeledStates + state

      state.next.values.foreach((s: State[T]) => writeState(s.asInstanceOf[AbstractState[T]]))
      writeState(state.default.asInstanceOf[AbstractState[T]])

    }

    try {
      file.write("digraph DerivableAutomation {\n")
      writeState(start.asInstanceOf[AbstractState[T]])
      file.write("}\n")
    } finally {
      file.close()
    }
  }
}
