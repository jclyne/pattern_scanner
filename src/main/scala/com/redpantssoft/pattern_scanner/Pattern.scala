package com.redpantssoft.pattern_scanner

/** Represents a Pattern in the Pattern Scanner
  *
  * @param id id of the pattern
  * @param regExp regular expression to identify the pattern
  */
@SerialVersionUID(1L)
case class Pattern(id: Pattern.Id,
                   name: String,
                   regExp: String,
                   ignore: Boolean = false) {

  override def toString = id + ": " + name + ", RegExp:" + regExp
}

object Pattern {

  /** Represents a pattern ID of the form {MAJOR}.{MINOR}
    *
    * @param major major portion of the ID
    * @param minor minor portion of the ID
    */
  case class Id(major: Long, minor: Long) {
    override def toString = major + "." + minor
  }

}
