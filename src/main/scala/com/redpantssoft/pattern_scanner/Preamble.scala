package com.redpantssoft.pattern_scanner


/** Package level implicit conversions */
object Preamble {

  /** Converts a Double, or the format MAJOR.MINOR to a Pattern ID
    *
    * @param d double to convert
    * @return new Pattern ID
    */
  implicit def double2PatternId(d: Double): Pattern.Id =
    d.toString.split('.') match {
      case Array(major, minor) => Pattern.Id(major.toInt, minor.toInt)
    }

  /** Converts a tuple, of the format(MAJOR,MINOR) to a Pattern ID
    *
    * @param v tuple to convert
    * @return  new Pattern ID
    */
  implicit def tuple2PatternId(v: (Int, Int)): Pattern.Id = Pattern.Id(v._1, v._2)
}
