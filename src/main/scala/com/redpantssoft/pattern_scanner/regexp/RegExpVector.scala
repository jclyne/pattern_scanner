package com.redpantssoft.pattern_scanner.regexp

//import collection.mutable.Set

/** Represents a Vector of Regular Expressions
  *
  * This implementation allows for generalizing a vector of  Derivable
  * RegExp as a single Derivable. This class is can passed to the
  * DerivableAutomaton factory to compile a state machine just as a
  * single Derivable regular expression
  *
  * @param vector list of RegExp
  */
case class RegExpVector(vector: List[RegExp]) extends Derivable[RegExpVector] {

  override def nullable = vector.exists(_.nullable)

  override def isFinal = vector.forall(_.isFinal)

  override def derive(c: Char): RegExpVector = new RegExpVector(vector.map(_.derive(c)))

  override def derive(): DerivationMap[RegExpVector] = {
    val v = vector.reverse
    var hdr = v.head.derive()
    var drList = hdr.list.map {
      case (dr, chars) => (new RegExpVector(List(dr)), chars)
    }
    var drDefault = new RegExpVector(List(hdr.default))

    for (hdr <- v.tail.map(_.derive())) {
      var common = Set[Char]()
      drList = (hdr.list.flatMap {
        case (dr, hchars) => drList.flatMap {
          case (drv, vchars) => (hchars & vchars) match {
            case chars if (chars.isEmpty) => Nil
            case chars => common |= chars; (new RegExpVector(dr :: drv.vector), chars) :: Nil
          }
        }
      }) :::
        (hdr.list.flatMap {
          case (dr, hchars) =>
            (hchars &~ common) match {
              case chars if chars.isEmpty => Nil
              case chars => (new RegExpVector(dr :: drDefault.vector), chars) :: Nil
            }
        }) :::
        (drList.flatMap {
          case (drv, vchars) =>
            (vchars &~ common) match {
              case chars if chars.isEmpty => Nil
              case chars => (new RegExpVector(hdr.default :: drv.vector), chars) :: Nil
            }
        })
      drDefault = new RegExpVector(hdr.default :: drDefault.vector)
    }
    DerivationMap(drList, drDefault)
  }

  override def equals(that: Any) = that match {
    case that: RegExpVector => this.vector == that.vector
    case _ => false
  }

  override lazy val hashCode = vector.hashCode()
}
