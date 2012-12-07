package com.redpantssoft.pattern_scanner.automaton

/** Represents a State in a Finite State Automaton
  *
  * Any state in the machine can be accepting or final. An accepting state is one that
  * indicates that the machine has accepted the input thus far. A final state indicates that
  * there is no condition that will transition the state to a different state. An accepting
  * final state indicates that the machine will always accept any condition. Likewise, the
  * non-accepting, final state  indicates that no condition will transition to a new state.
  *
  * @tparam T  - type of the transition input
  */
trait State[T] {

  def id: State.Id

  def isFinal: Boolean

  def isAccepting: Boolean

  /** Accepts input and transitions to new state */
  def input(c: T): State[T]

}

object State {
  type Id = Int
}

/** Provides an interface for state machine construction logic to update existing states.
  *
  * This allows for separation of the definition of a state and construction its map
  * of transitions.  Numerous state machine construction algorithms make use of graph
  * traversal requiring the nodes to exist prior to edges defined.
  *
  * @param id  id of the underlying state
  * @param isAccepting indication that the underlying state is accepting
  * @param isFinal indication that the underlying state is final
  */
@SerialVersionUID(1)
sealed private[automaton]
class AbstractState[T: Manifest](override val id: State.Id = -1,
                                 override val isAccepting: Boolean = false,
                                 override val isFinal: Boolean = false) extends State[T] with Serializable {


  require(id >= 0, "State ID's must be an integer >= 0")

  private[automaton] var next: Map[T, State[T]] = Map[T, State[T]]()
  private[automaton] var default: State[T] = UndefinedState.asInstanceOf[State[T]]

  override def input(c: T): State[T] = next.getOrElse(c, default)

  override def equals(that: Any) = that match {
    case that: AnyRef if manifest[T].erasure == that.getClass => id == that.asInstanceOf[AbstractState[T]].id
    case _ => false
  }

  override def hashCode = id.hashCode
}

/** Defines an empty state that is final and non-accepting */
object FinalState extends State[Any] {
  def id = -1

  def isFinal = true

  def isAccepting = false

  /** Accepts input and transitions to new state */
  def input(c: Any) = FinalState
}

/** Defines an undefined state on Any transition input type */
object UndefinedState extends State[Any] {

  override val id = -1

  override val isFinal = false

  override val isAccepting = false

  @throws(classOf[UndefinedStateException])
  override def input(c: Any): State[Any] =
    throw new UndefinedStateException("input " + c)
}

/** Exception that occurs when attempting a state transition on an undefined state
  *
  * @param message specified detailed message
  * @param cause cause of the exception
  */
class UndefinedStateException(message: String, cause: Throwable) extends Exception {

  /** Constructs a new exception with null as its detail message. */
  def this() = this(UndefinedStateException.msgPrefix, null)

  /** Constructs a new exception with the specified detail message. */
  def this(message: String) = this(UndefinedStateException.msgPrefix + ": " + message, null)

  /** Constructs a new exception with the specified cause and a detail message of (
    * cause==null ? null : cause.toString())
    */
  def this(cause: Throwable) = this(if (cause == null) null else cause.toString, cause)
}

object UndefinedStateException {
  /** Prefix for UndefinedStateException messages */
  val msgPrefix = "Operation attempted on an undefined com.redpantssoft.pattern_scanner.automaton state"
}








