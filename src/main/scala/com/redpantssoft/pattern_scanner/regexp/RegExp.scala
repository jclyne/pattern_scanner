package com.redpantssoft.pattern_scanner.regexp


/** Implementation of Regular Expression Derivatives
  * Taken from "Regular Expression Derivatives Reexamined" -Scott Owens, John Reppy,
  * Aaron Turon, In Journal of Functional Programming, March 2009, vol 19, issue 02, pp. 173-190
  *
  * The derivative of a set of strings S with respect to a symbol a is the set of strings generated
  * by stripping the leading a from the strings in S that start with a. For regular sets of
  * strings, i.e., sets defined by regular expressions (REs), the derivative is also a regular set. In
  * a 1964 paper, Janusz Brzozowski presented an elegant method for directly constructing a
  * recognizer from a regular expression based on regular-expression derivatives (Brzozowski,
  * 1964).
  *
  */


abstract class RegExp extends Derivable[RegExp] {

  /** Matches this followed by r
    *
    * @param r RegExp to concatenation with this RegExp
    * @return new Concatention RegExp
    */
  def ++(r: RegExp): RegExp = Cat(this, r)

  /** Matches this or r
    *
    * @param r RegExp to OR with this RegExp
    * @return new Or RegExp
    */
  def ||(r: RegExp): RegExp = Or(this, r)

  /** Matches this and r
    *
    * @param r RegExp to And with this RegExp
    * @return new And RegExp
    */
  def &&(r: RegExp): RegExp = And(this, r)

  /** Matches any symbol between this and r
    *
    * @param r Symbol that is the Range Max
    * @return new Or RegExp
    */
  def --(r: RegExp): RegExp = {
    require(isInstanceOf[Symbol], "Range operator only valid on character literals")
    require(r.isInstanceOf[Symbol], "Range operator only valid on character literals")

    val min = this.asInstanceOf[Symbol].c
    val max = r.asInstanceOf[Symbol].c
    require(max > min, "Range max must be greater than range min")

    val range = (min to max).map(Preamble.char2Symbol)
    (range.head /: range.tail)(Or(_, _))
  }

  /** Matches this zero or more times
    *
    * @return new Star RegExp
    */
  def * = Star(this)

  /** Matches this n times
    *
    * @param n number of times to match this
    * @return new Cat RegExp
    */
  def *(n: Int): RegExp = (this /: (1 until n))((ret, i) => Cat(ret, this))

  /** Matches this at least m times but no more than n times
    *
    * @param m min number of times to match this
    * @param n max number of times to match this
    * @return new Cat RegExp
    */
  def *(m: Int, n: Int): RegExp = {
    require(m <= n, "Range max must be less than range min")
    if (m == n) *(m)
    else Cat(*(m), (this.? /: (m + 1 until n))((ret, i) => Cat(ret, this.?)))
  }

  /** Matches this one or more times
    *
    * @return new Cat RegExp
    */
  def + = Cat(this, Star(this))

  /** Matches this zero or one time
    *
    * @return new Or RegExp
    */
  def ? = Or(emptyStr, this)

  /** Matches not this
    *
    * @return new Not RegExp
    */
  def unary_! = Not(this)


  /** Indicates whether or not this RegExp is final
    *
    * @return whether the derivative is final
    */
  override def isFinal = this == emptySet
}

object RegExp {
  /** Produces a RegExp from a string representation of the expression
    *
    * @param s string representation of the RegExp
    * @return newly constructed RegExp
    */
  def apply(s: String): RegExp = RegExpCompiler.parse(s)
}

/** Represents the empty set regular expression
  * Empty Set represents an expression that matches nothing
  */
case object emptySet extends RegExp {
  override def nullable = false

  override def derive(c: Char) = emptySet

  override def derive() = DerivationMap(Nil, emptySet)

  override def toString = "\u0398"
}

/** Represents the empty string regular expression
  * Empty String represents an expression that matches no input. In other
  * words, it matches what was already input
  */
case object emptyStr extends RegExp {
  override def nullable = true

  override def derive(c: Char) = emptySet

  override def derive() = DerivationMap(Nil, emptySet)

  override def toString = "\u03A3"
}

/** Represents any symbol */
case object any extends RegExp {
  override def nullable = false

  override def derive(a: Char) = emptyStr

  override def derive() = DerivationMap(Nil, emptyStr)
}

/** Represents the symbol specified by the character c */
case class Symbol(c: Char) extends RegExp {
  override def nullable = false

  override def derive(a: Char) = if (a == c) emptyStr else emptySet

  override def derive() = DerivationMap(List((emptyStr, Set(c))), emptySet)
}

/** Represents a RegExp that repeats r zero or more times
  *
  * @param r RegExp to repeat zero or more times
  */
class Star private(val r: RegExp) extends RegExp {
  override def nullable = true

  private def buildDerivative(rdr: RegExp): RegExp =
    Cat(rdr, this)

  override def derive(c: Char) = buildDerivative(r.derive(c))

  override def derive() = {
    val rdr = r.derive()
    DerivationMap(rdr.list.map {
      case (dr, chars) => (buildDerivative(dr), chars)
    }, rdr.default)
  }

  override def equals(that: Any) = that match {
    case that: Star => r.equals(that.r)
    case _ => false
  }

  private val prime = 41
  override lazy val hashCode = prime * (prime + r.hashCode())

  override def toString = "Star(" + r.toString + ")"
}

object Star {
  /** Factory method
    * Builds a new RegExp that may return a reduced equivalent RegExp
    */
  def apply(r: RegExp): RegExp = r match {
    case r: Star => r
    case `emptyStr` => emptyStr
    case `emptySet` => emptyStr
    case _ => new Star(r)
  }

  def unapply(obj: Star): Option[(RegExp)] = Some(obj.r)
}

/** Represents the Concatenation of two RegExp
  *
  * @param r RegExp to concatenate
  * @param s RegExp to concatenate
  */
class Cat private(val r: RegExp, val s: RegExp) extends RegExp {
  override def nullable = r.nullable && s.nullable

  private def buildDerivative(rdr: RegExp, sdr: RegExp): RegExp =
    (if (r.nullable)
      Or(Cat(rdr, s), sdr)
    else
      Cat(rdr, s))

  override def derive(c: Char) = buildDerivative(r.derive(c), s.derive(c))

  override def derive() = {
    val (rdr_map, sdr_map) = (r.derive(), s.derive())
    val default = buildDerivative(rdr_map.default, sdr_map.default)
    var common = Set[Char]()
    DerivationMap((rdr_map.list.flatMap {
      case (rdr, rchars) => sdr_map.list.flatMap {
        case (sdr, schars) =>
          (rchars & schars) match {
            case chars if chars.isEmpty => Nil
            case chars =>
              common |= chars
              (buildDerivative(rdr, sdr), chars) :: Nil
          }
      }
    }) :::
      (rdr_map.list.flatMap {
        case (rdr, rchars) =>
          (rchars &~ common) match {
            case chars if chars.isEmpty => Nil
            case chars =>
              (buildDerivative(rdr, sdr_map.default), chars) :: Nil
          }
      }) :::
      (sdr_map.list.flatMap {
        case (sdr, schars) =>
          (schars &~ common) match {
            case chars if chars.isEmpty => Nil
            case chars =>
              if (r.nullable)
                (buildDerivative(rdr_map.default, sdr), chars) :: Nil
              else
                Nil
          }
      }), default)
  }

  override def equals(that: Any) = (this, that) match {
    case (Cat(this_r, this_s), Cat(that_r, that_s))
      if (this_r, this_s) ==(that_r, that_s) => true

    case (Cat(Cat(this_r, this_s), this_t), Cat(that_r, Cat(that_s, that_t)))
      if (this_r, this_s, this_t) ==(that_r, that_s, that_t) => true

    case (Cat(this_r, Cat(this_s, this_t)), Cat(Cat(that_r, that_s), that_t))
      if (this_r, this_s, this_t) ==(that_r, that_s, that_t) => true

    case _ => false
  }

  private val prime = 61
  override lazy val hashCode = {
    def findCatTerms(r: Any): List[RegExp] = r match {
      case Cat(r: Cat, s: Cat) => findCatTerms(r) ::: findCatTerms(s)
      case Cat(r: Cat, s: RegExp) => findCatTerms(r) ::: (s :: Nil)
      case Cat(r: RegExp, s: Cat) => r :: findCatTerms(s)
      case Cat(r: RegExp, s: RegExp) => r :: s :: Nil
      case r: RegExp => r :: Nil
    }
    (prime /: findCatTerms(this))((prev, term) => prime * (prev + term.hashCode()))
  }

  override def toString = "Cat(" + r.toString + "," + s.toString + ")"
}

object Cat {
  /** Factory method
    * Builds a new RegExp that may return a reduced equivalent RegExp
    */
  def apply(r: RegExp, s: RegExp): RegExp = (r, s) match {
    case (`emptySet`, r: RegExp) => emptySet
    case (r: RegExp, `emptySet`) => emptySet
    case (`emptyStr`, r: RegExp) => r
    case (r: RegExp, `emptyStr`) => r
    case _ => new Cat(r, s)
  }

  def unapply(obj: Cat): Option[(RegExp, RegExp)] = Some((obj.r, obj.s))
}

/** Represents a logical or (alternation)  of RegExp
  *
  * @param r optional RegExp
  * @param s optional RegExp
  */
class Or private(val r: RegExp, val s: RegExp) extends RegExp {
  override def nullable = r.nullable || s.nullable


  private def buildDerivative(rdr: RegExp, sdr: RegExp): RegExp =
    Or(rdr, sdr)

  override def derive(c: Char) = buildDerivative(r.derive(c), s.derive(c))

  override def derive() = {
    val (rdr_map, sdr_map) = (r.derive(), s.derive())
    val default = buildDerivative(rdr_map.default, sdr_map.default)
    var common = Set[Char]()
    DerivationMap((rdr_map.list.flatMap {
      case (rdr, rchars) => sdr_map.list.flatMap {
        case (sdr, schars) =>
          (rchars & schars) match {
            case chars if chars.isEmpty => Nil
            case chars => common |= chars; (buildDerivative(rdr, sdr), chars) :: Nil
          }
      }
    }) :::
      (rdr_map.list.flatMap {
        case (rdr, rchars) =>
          (rchars &~ common) match {
            case chars if chars.isEmpty => Nil
            case chars => (buildDerivative(rdr, sdr_map.default), chars) :: Nil
          }
      }) :::
      (sdr_map.list.flatMap {
        case (sdr, schars) =>
          (schars &~ common) match {
            case chars if chars.isEmpty => Nil
            case chars => (buildDerivative(rdr_map.default, sdr), chars) :: Nil
          }
      }), default)
  }

  override def equals(that: Any) = (this, that) match {

    case (Or(this_r, this_s), Or(that_r, that_s))
      if Set(this_r, this_s) == Set(that_r, that_s) => true

    case (Or(Or(this_r, this_s), this_t), Or(that_r, Or(that_s, that_t)))
      if Set(this_r, this_s, this_t) == Set(that_r, that_s, that_t) => true

    case (Or(this_r, Or(this_s, this_t)), Or(Or(that_r, that_s), that_t))
      if Set(this_r, this_s, this_t) == Set(that_r, that_s, that_t) => true

    case _ => false
  }

  private val prime = 113
  override lazy val hashCode = (prime + r.hashCode()) + (prime + s.hashCode())

  override def toString = "Or(" + r.toString + "," + s.toString + ")"
}

object Or {
  /** Factory method
    * Builds a new RegExp that may return a reduced equivalent RegExp
    */
  def apply(r: RegExp, s: RegExp): RegExp = (r, s) match {
    case (r: RegExp, s: RegExp) if r == s => r
    case (Not(`emptySet`), r: RegExp) => Not(`emptySet`)
    case (r: RegExp, Not(`emptySet`)) => Not(`emptySet`)
    case (`emptySet`, r: RegExp) => r
    case (r: RegExp, `emptySet`) => r
    case _ => new Or(r, s)
  }

  def unapply(obj: Or): Option[(RegExp, RegExp)] = Some((obj.r, obj.s))
}

/** Represents a logical and of  RegExp
  *
  * @param r RegExp
  * @param s RegExp
  */
class And private(val r: RegExp, val s: RegExp) extends RegExp {
  override def nullable = r.nullable && s.nullable

  private def buildDerivative(rdr: RegExp, sdr: RegExp): RegExp =
    And(rdr, sdr)

  override def derive(c: Char) = buildDerivative(r.derive(c), s.derive(c))

  override def derive() = {
    val (rdr_map, sdr_map) = (r.derive(), s.derive())
    val default = buildDerivative(rdr_map.default, sdr_map.default)
    var common = Set[Char]()
    DerivationMap((rdr_map.list.flatMap {
      case (rdr, rchars) => sdr_map.list.flatMap {
        case (sdr, schars) =>
          (rchars & schars) match {
            case chars if chars.isEmpty => Nil
            case chars => common |= chars; (buildDerivative(rdr, sdr), chars) :: Nil
          }
      }
    }) :::
      (rdr_map.list.flatMap {
        case (rdr, rchars) =>
          (rchars &~ common) match {
            case chars if chars.isEmpty => Nil
            case chars => (buildDerivative(rdr, sdr_map.default), chars) :: Nil
          }
      }) :::
      (sdr_map.list.flatMap {
        case (sdr, schars) =>
          (schars &~ common) match {
            case chars if chars.isEmpty => Nil
            case chars => (buildDerivative(rdr_map.default, sdr), chars) :: Nil
          }
      }), default)
  }

  override def equals(that: Any) = (this, that) match {
    case (And(this_r, this_s), And(that_r, that_s))
      if Set(this_r, this_s) == Set(that_r, that_s) => true

    case (And(And(this_r, this_s), this_t), And(that_r, And(that_s, that_t)))
      if Set(this_r, this_s, this_t) == Set(that_r, that_s, that_t) => true

    case (And(this_r, And(this_s, this_t)), And(And(that_r, that_s), that_t))
      if Set(this_r, this_s, this_t) == Set(that_r, that_s, that_t) => true

    case _ => false
  }

  private val prime = 181
  override lazy val hashCode = (prime + r.hashCode()) + (prime + s.hashCode())

  override def toString = "And(" + r.toString + "," + s.toString + ")"
}

object And {
  /** Factory method
    * Builds a new RegExp that may return a reduced equivalent RegExp
    */
  def apply(r: RegExp, s: RegExp): RegExp = (r, s) match {
    case (r: RegExp, s: RegExp) if r == s => r
    case (Not(`emptySet`), r: RegExp) => r
    case (r: RegExp, Not(`emptySet`)) => r
    case (`emptySet`, r: RegExp) => emptySet
    case (r: RegExp, `emptySet`) => emptySet
    case _ => new And(r, s)
  }

  def unapply(obj: And): Option[(RegExp, RegExp)] = Some((obj.r, obj.s))
}

/** Represents an negated RegExp
  *
  * @param r RegExp to negate
  */
class Not private(val r: RegExp) extends RegExp {
  override def nullable = !r.nullable

  private def buildDerivative(rdr: RegExp): RegExp = Not(rdr)

  override def derive(c: Char) = buildDerivative(r.derive(c))

  override def derive() = {
    val rdr = r.derive()
    DerivationMap(
      rdr.list.map {
        case (dr, chars) => (buildDerivative(dr), chars)
      }, buildDerivative(rdr.default))
  }

  override def equals(that: Any) = that match {
    case that: Not => r.equals(that.r)
    case _ => false
  }

  private val prime = 313
  override lazy val hashCode = prime * (prime + r.hashCode())

  override def toString = "Not(" + r.toString + ")"
}

object Not {
  /** Factory method
    * Builds a new RegExp that may return a reduced equivalent RegExp
    */
  def apply(r: RegExp): RegExp = r match {
    case Not(r: RegExp) => r
    case _ => new Not(r)
  }

  def unapply(obj: Not): Option[RegExp] = Some(obj.r)

}

