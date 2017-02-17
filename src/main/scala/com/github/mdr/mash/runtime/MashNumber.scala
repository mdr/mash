package com.github.mdr.mash.runtime

import com.github.mdr.mash.classes.MashClass
import com.github.mdr.mash.utils.NumberUtils

import scala.PartialFunction.cond

object MashNumber {

  def apply(n: Any, tagClass: MashClass): MashNumber = apply(n, Some(tagClass))

  def apply(n: Any, tagClassOpt: Option[MashClass]): MashNumber =
    n match {
      case n: Double ⇒ MashNumber(n, tagClassOpt)
      case n: Float  ⇒ MashNumber(n.toDouble, tagClassOpt)
      case n: Int    ⇒ MashNumber(n.toDouble, tagClassOpt)
      case n: Long   ⇒ MashNumber(n.toDouble, tagClassOpt)
      case _         ⇒ throw new IllegalArgumentException("Unexpected number type: " + n)
    }

  def min(n1: MashNumber, n2: MashNumber): MashNumber =
    if (n1.compareTo(n2) <= 0) n1 else n2

  def max(n1: MashNumber, n2: MashNumber): MashNumber =
    if (n1.compareTo(n2) >= 0) n1 else n2

}

case class MashNumber(n: Double, tagClassOpt: Option[MashClass] = None) extends TaggableMashValue with Comparable[MashNumber] {

  def compareTo(that: MashNumber) = this.n compareTo that.n

  def +(that: MashNumber): MashNumber = copy(n = this.n + that.n, tagClassOpt = this.tagClassOpt orElse that.tagClassOpt)

  def -(that: MashNumber): MashNumber = copy(n = this.n - that.n, tagClassOpt = this.tagClassOpt orElse that.tagClassOpt)

  def *(that: MashNumber): MashNumber = copy(n = this.n * that.n, tagClassOpt = this.tagClassOpt orElse that.tagClassOpt)

  def /(that: MashNumber): MashNumber = copy(n = this.n / that.n, tagClassOpt = this.tagClassOpt orElse that.tagClassOpt)

  def negate: MashNumber = copy(n = -this.n)

  def asInt: Option[Int] = if (n.isValidInt) Some(n.toInt) else None

  def isInt = asInt.isDefined

  def withTag(tagClass: MashClass) = copy(tagClassOpt = Some(tagClass))

  def modify(f: Double ⇒ Double): MashNumber = copy(n = f(n))

  override def equals(that: Any) = cond(that) {
    case that: MashNumber ⇒ this.n == that.n
  }

  override def hashCode = n.hashCode

  override def toString = NumberUtils.prettyString(n)

}

object MashInteger {

  def unapply(x: MashValue): Option[Int] = x match {
    case n: MashNumber ⇒ n.asInt
    case _             ⇒ None
  }

}