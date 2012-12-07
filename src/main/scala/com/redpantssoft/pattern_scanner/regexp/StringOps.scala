package com.redpantssoft.pattern_scanner.regexp

import Preamble._

/** Extension class for Strings that adds toRegExp conversion
  *
  * @param s String to wrap
  */
class StringOps(s: String) {
  /**
   * Converts wrapped string to RegExp using
   * implicit str2RegExp
   *
   * @return new RegExp representing the String s
   */
  def toRegExp = str2RegExp(s)
}