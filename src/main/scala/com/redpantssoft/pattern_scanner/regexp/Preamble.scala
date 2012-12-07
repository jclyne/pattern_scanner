package com.redpantssoft.pattern_scanner.regexp

/** Implicit definitions for package */
object Preamble {

  /** Implicit conversion from character literal to Symbol
    *
    * A Symbol is RegExp representing a single character literal
    *
    * @param c character to convert to a Symbol
    * @return new Symbol representing the character literal
    */
  implicit def char2Symbol(c: Char): RegExp = Symbol(c)

  /** Implicit conversion from integer to Symbol
    *
    * A Symbol is RegExp representing a single character literal.
    * This routine will first attempt to convert the integer to
    * Char and then create a Symbol
    *
    * @param i integer to convert to a Symbol
    * @return new Symbol representing the integer
    */
  implicit def int2Symbol(i: Int): RegExp = Symbol(i.toChar)

  /** Implicit conversion from String to RegExp
    *
    * A string is represented as a RegExp of each character,
    * converted to a Symbol, and concatenated together
    *
    * @param s string to convert to a RegExp
    * @return new RegExp representing the String s
    */
  implicit def str2RegExp(s: String): RegExp = s.headOption match {
    case None => emptyStr
    case Some(c) =>
      if (s.tail.isEmpty)
        Symbol(c)
      else
        Cat(Symbol(c), str2RegExp(s.tail))
  }

  /** Implicit conversion from string to StringOps wrapper
    *
    * @param s string to wrap with a StringOps
    * @return new StringOps wrapping the String s
    */
  implicit def str2Ops(s: String): StringOps = new StringOps(s)

  /** Implicit conversion from List[RegExp] to RegExpVector wrapper
    *
    * @param li List[RegExp] to wrap with RegExpVector
    * @return RegExpVector wrapping the List[RegExp] li
    */
  implicit def list2RegExpVector[T](li: List[RegExp]): RegExpVector = new RegExpVector(li)


  /**
   * Standard regexp definitions
   */

  /** Lowercase letters */
  val lower: RegExp = 'a' -- 'z'
  /** Uppercase  letters */
  val upper: RegExp = 'A' -- 'Z'
  /** Digits */
  val digit: RegExp = '0' -- '9'
  /** Hexadecimal digits */
  val xdigit: RegExp = ('A' -- 'F') || ('a' -- 'f') || digit
  /** Alphabetic  characters */
  val alpha: RegExp = upper || lower
  /** Alphanumeric characters */
  val alnum: RegExp = alpha || digit
  /** Alphanumeric characters plus "_" */
  val word: RegExp = alnum || '_'
  /** Punctuation characters */
  val punct: RegExp = ']' || '[' || '!' || '"' || '#' || '$' || '%' || '&' || '\'' ||
    '(' || ')' || '*' || '+' || ',' || '.' || '/' || ':' || ';' || '<' || '=' ||
    '>' || '?' || '@' || '^' || '_' || '`' || '{' || '|' || '}' || '~' || '-'
  /** Space and tab */
  val blank: RegExp = ' ' || '\t'
  /** Whitespace */
  val space: RegExp = blank || '\r' || '\n' || '\f'
  /** Control characters */
  val cntrl: RegExp = (0x00 -- 0x1F) || 0x7F
  /** Visible characters */
  val graph: RegExp = (0x21 -- 0x7E)
  /** Visible characters and the space character */
  val print: RegExp = 0x20 || graph

  /**
   * Extended regexp definitions
   */

  /** Optional numeric sign */
  val sign: RegExp = ("+" || "-").?
  /** One or more digits */
  val digits: RegExp = digit.+
  /** Integer */
  val int: RegExp = sign ++ digits
  /** One or more hexadecimal  digits */
  val xdigits: RegExp = xdigit.+
  /** Hexadecimal integer */
  val hex: RegExp = sign ++ "0x".? ++ xdigits

}
