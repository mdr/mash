package com.github.mdr.mash.evaluator

import scala.PartialFunction.cond

import com.github.mdr.mash.utils.NumberUtils

object MashNumber {

  def apply(n: Any, tagClassOpt: Option[MashClass]): MashNumber = n match {
    case n: Double ⇒ MashNumber(n, tagClassOpt)
    case n: Float  ⇒ MashNumber(n.toDouble, tagClassOpt)
    case n: Int    ⇒ MashNumber(n.toDouble, tagClassOpt)
    case n: Long   ⇒ MashNumber(n.toDouble, tagClassOpt)
  }

  def min(n1: MashNumber, n2: MashNumber): MashNumber =
    if (n1.compareTo(n2) <= 0)
      n1
    else
      n2

  def max(n1: MashNumber, n2: MashNumber): MashNumber =
    if (n1.compareTo(n2) >= 0)
      n1
    else
      n2

}

case class MashNumber(n: Double, tagClassOpt: Option[MashClass] = None) extends Comparable[MashNumber] {

  override def toString = NumberUtils.prettyString(n)

  def compareTo(that: MashNumber) = n.compareTo(that.n)

  def +(that: MashNumber): MashNumber = copy(n = this.n + that.n)

  def -(that: MashNumber): MashNumber = copy(n = this.n - that.n)

  def *(that: MashNumber): MashNumber = copy(n = this.n * that.n)

  def /(that: MashNumber): MashNumber = copy(n = this.n / that.n)

  def negate: MashNumber = copy(n = -this.n)

  def asInt: Option[Int] = if (n.isValidInt) Some(n.toInt) else None

  override def equals(that: Any) = cond(that) {
    case MashNumber(n2, _) ⇒ n == n2
  }

  override def hashCode = n.hashCode

  def withTag(tagClass: MashClass) = copy(tagClassOpt = Some(tagClass))

}
