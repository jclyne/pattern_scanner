package com.redpantssoft.pattern_scanner.regexp

import util.parsing.combinator.RegexParsers
import Preamble._

/** Defines a parser for regular expression strings
  *
  * {{{
  * The syntax follows the Posix extended regular expressions, without
  * support for Start of String (^) or End of string ($, as  this is intended
  * for regular expression derivatives in a scanner.
  *
  * Syntax:
  *  .       matches an single character
  *  [ ]     matches a single character contained in the brackets
  *  [^ ]    matches a single character that is not contained within the brackets
  *  *       matches the preceding element zero or more times
  * {m}      matches the preceding element exactly m times
  * {m,n}   matches the preceding element at least m and not more than n times
  * ?       matches the preceding element zero or one time
  * +       matches the preceding element one or more times
  * |       matches either the expression before or the expression after the operator
  *
  * Character Classes:
  * [:alnum:]      [A-Za-z0-9]       Alphanumeric characters
  * [:word:]  \w   [A-Za-z0-9_]	    Alphanumeric characters plus "_"
  * [:alpha:] \a	  [A-Za-z]	        Alphabetic characters
  * [:blank:]	    [ \t]	            Space and tab
  * [:cntrl:]	    [\x00-\x1F\x7F]	  Control characters
  * [:digit:] \d	  [0-9]	            Digits
  * \D	            [^0-9]	          Non-digits
  * [:graph:]		  [\x21-\x7E]	      Visible characters
  * [:lower:]		  [a-z]	            Lowercase letters
  * [:print:]		  [\x20-\x7E]	      Visible characters and the space character
  * [:punct:]		  [\]\[!"#$%&'()*+,./:;<=>?@\^_`{|}~-]	Punctuation characters
  * [:space:] \s   [ \t\r\n\v\f]	    Whitespace characters
  * \S	            [^ \t\r\n\v\f]	  Non-whitespace characters
  * [:upper:]      [A-Z]	            Uppercase letters
  * [:xdigit:]\x	  [A-Fa-f0-9]	      Hexadecimal digits
  *
  * }}}
  */
private[regexp] object RegExpCompiler extends RegexParsers {


  override protected val whiteSpace = "".r

  def expr: Parser[RegExp] = union | expr01

  def union: Parser[RegExp] = expr01 ~ "|" ~ expr01 ^^ {
    case r ~ "|" ~ s => r || s
  }

  def expr01: Parser[RegExp] = rep(expr02) ^^ {
    case Nil => emptySet
    case head :: Nil => head
    case head :: tail => (head /: tail)(_ ++ _)
  }

  def expr02: Parser[RegExp] = star | plus | question | count | min_max | expr03

  def count: Parser[RegExp] = expr03 ~ "{" ~ """[0-9]+""".r ~ "}" ^^ {
    case r ~ "{" ~ count ~ "}" => r.*(count.toInt)
  }

  def min_max: Parser[RegExp] = expr03 ~ "{" ~ """[0-9]+""".r ~ "," ~ """[0-9]+""".r ~ "}" ^^ {
    case r ~ "{" ~ min ~ "," ~ max ~ "}" => r.*(min.toInt, max.toInt)
  }

  def star: Parser[RegExp] = expr03 ~ "*" ^^ {
    case r ~ "*" => r.*
  }

  def plus: Parser[RegExp] = expr03 ~ "+" ^^ {
    case r ~ "+" => r.+
  }

  def question: Parser[RegExp] = expr03 ~ "?" ^^ {
    case r ~ "?" => r.?
  }

  def expr03: Parser[RegExp] = group | dot | char | char_class | set

  def group: Parser[RegExp] = "(" ~ expr ~ ")" ^^ {
    case "(" ~ expr ~ ")" => (expr)
  }

  def dot: Parser[RegExp] = "." ^^ {
    case _ => any
  }

  def set: Parser[RegExp] = set_intersection | set_union | char_set

  def set_intersection: Parser[RegExp] = char_set ~ "{-}" ~ char_set ^^ {
    case s1 ~ "{-}" ~ s2 => And(s1, Not(s2))
  }

  def set_union: Parser[RegExp] = char_set ~ "{+}" ~ char_set ^^ {
    case s1 ~ "{+}" ~ s2 => Or(s1, s2)
  }

  def char_set: Parser[RegExp] = negative_set | positive_set

  def positive_set: Parser[RegExp] = "[" ~ rep(set_item) ~ "]" ^^ {
    case "[" ~ items ~ "]" => items match {
      case Nil => emptySet
      case head :: Nil => head
      case head :: tail => (head /: tail)(_ || _)
    }
  }

  def negative_set: Parser[RegExp] = "[^" ~ rep(set_item) ~ "]" ^^ {
    case "[^" ~ items ~ "]" => items match {
      case Nil => emptySet
      case head :: Nil => !head && any
      case head :: tail => !((head /: tail)(_ || _)) && any
    }
  }

  def set_item: Parser[RegExp] = range | char | char_class | valid_set_meta_chars

  def range: Parser[RegExp] = char ~ "-" ~ char ^^ {
    case min ~ "-" ~ max => min -- max
  }

  def char: Parser[RegExp] = not_meta_char | escaped_ctrl_char | escaped_meta_char


  val meta_chars = """|?+*.$^(){}\\\[\]-"""

  def not_meta_char: Parser[RegExp] = ("[^" + meta_chars + "]").r ^^ {
    case s => s
  }

  def valid_set_meta_chars: Parser[RegExp] = ("""[$()*+?^{}\[\-\\]""").r ^^ {
    case s => s
  }

  def escaped_meta_char: Parser[RegExp] = """\""" ~ ("[" + meta_chars + "]").r ^^ {
    case """\""" ~ s => s
  }

  def escaped_ctrl_char: Parser[RegExp] = """\""" ~ ("[abfnrtv ]").r ^^ {
    case """\""" ~ "b" => "\b"
    case """\""" ~ "f" => "\f"
    case """\""" ~ "n" => "\n"
    case """\""" ~ "r" => "\r"
    case """\""" ~ "t" => "\t"
    case """\""" ~ " " => " "
  }

  def char_class = (alnum | not_alnum | word | not_word | alpha | not_alpha | blank | not_blank |
    cntrl | not_cntrl | digit | not_digit | graph | not_graph | lower | not_lower | print | not_print |
    punct | not_punct | space | not_space | upper | not_upper | xdigit | not_xdigit)

  def alnum: Parser[RegExp] = "[:alnum:]" ^^ {
    case _ => Preamble.alnum
  }

  def not_alnum: Parser[RegExp] = "[:^alnum:]" ^^ {
    case _ => Not(Preamble.alnum) && any
  }

  def word: Parser[RegExp] = ("[:word:]" | """\w""") ^^ {
    case _ => Preamble.word
  }

  def not_word: Parser[RegExp] = """\W""" ^^ {
    case _ => Not(Preamble.word) && any
  }

  def alpha: Parser[RegExp] = ("[:alpha:]" | """\a""") ^^ {
    case _ => Preamble.alpha
  }

  def not_alpha: Parser[RegExp] = "[:^alpha:]" ^^ {
    case _ => Not(Preamble.alpha) && any
  }

  def blank: Parser[RegExp] = "[:blank:]" ^^ {
    case _ => Preamble.blank
  }

  def not_blank: Parser[RegExp] = "[:^blank:]" ^^ {
    case _ => Not(Preamble.blank) && any
  }

  def cntrl: Parser[RegExp] = "[:cntrl:]" ^^ {
    case _ => Preamble.cntrl
  }

  def not_cntrl: Parser[RegExp] = "[:^cntrl:]" ^^ {
    case _ => Not(Preamble.cntrl) && any
  }

  def digit: Parser[RegExp] = ("[:digit:]" | """\d""") ^^ {
    case _ => Preamble.digit
  }

  def not_digit: Parser[RegExp] = ("[:^digit:]" | """\D""") ^^ {
    case _ => Not(Preamble.digit) && any
  }

  def graph: Parser[RegExp] = "[:graph:]" ^^ {
    case _ => Preamble.graph
  }

  def not_graph: Parser[RegExp] = "[:^graph:]" ^^ {
    case _ => Not(Preamble.graph) && any
  }

  def lower: Parser[RegExp] = "[:lower:]" ^^ {
    case _ => Preamble.lower
  }

  def not_lower: Parser[RegExp] = "[:^lower:]" ^^ {
    case _ => Not(Preamble.lower) && any
  }

  def print: Parser[RegExp] = "[:print:]" ^^ {
    case _ => Preamble.print
  }

  def not_print: Parser[RegExp] = "[:^print:]" ^^ {
    case _ => Not(Preamble.print) && any
  }

  def punct: Parser[RegExp] = "[:punct:]" ^^ {
    case _ => Preamble.punct
  }

  def not_punct: Parser[RegExp] = "[:^punct:]" ^^ {
    case _ => Not(Preamble.punct) && any
  }

  def space: Parser[RegExp] = ("[:space:]" | """\s""") ^^ {
    case _ => Preamble.space
  }

  def not_space: Parser[RegExp] = ("[:^space:]" | """\S""") ^^ {
    case _ => Not(Preamble.space) && any
  }

  def upper: Parser[RegExp] = "[:upper:]" ^^ {
    case _ => Preamble.upper
  }

  def not_upper: Parser[RegExp] = "[:^upper:]" ^^ {
    case _ => Not(Preamble.upper) && any
  }

  def xdigit: Parser[RegExp] = ("[:xdigit:]" | """\x""") ^^ {
    case _ => Preamble.xdigit
  }

  def not_xdigit: Parser[RegExp] = ("[:^xdigit:]" | """\x""") ^^ {
    case _ => Not(Preamble.xdigit) && any
  }

  /** Compiles the specified string into a RegExp
    *
    * @param s regular expression string to compile
    * @return compiled RegExp object
    */
  def parse(s: String): RegExp = RegExpCompiler.parseAll(expr, s) match {
    case Success(result, rest) if rest.atEnd => result
    case Success(result, rest) =>
      throw new RegExpParserException(
        "Could not completely parse the input",
        rest.source.toString
      )
    case NoSuccess(msg, rest) => throw new RegExpParserException(msg)
  }
}

/** Exception that occurs when attempting to parse an invalid regular expression string
  *
  * @param message specified detailed message
  * @param rest specified rest of the string that failed to parse
  * @param cause cause of the exception
  */
class RegExpParserException(message: String, val rest: String, cause: Throwable)
  extends Exception(message, cause) {

  /** Constructs a new exception with null as its detail message. */
  def this() = this("RegExpParserException", "", null)

  /** Constructs a new exception with the specified detail message. */
  def this(message: String) = this(message, "", null)

  /** Constructs a new exception with the specified detail message and the rest of the input. */
  def this(message: String, rest: String) = this(message, rest, null)

  /** Constructs a new exception with the specified cause and a detail message of (
    * cause==null ? null : cause.toString())
    */
  def this(cause: Throwable) = this(if (cause == null) null else cause.toString, "", cause)
}

