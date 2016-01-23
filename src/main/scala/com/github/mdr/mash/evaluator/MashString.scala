package com.github.mdr.mash.evaluator

import scala.PartialFunction.cond

case class MashString(s: String, tagClassOpt: Option[MashClass] = None) extends Comparable[MashString] {

  def lookup(i: Int) = {
    val index = if (i < 0) i + s.size else i
    copy(s(index).toString)
  }

  def +(that: Any): MashString = copy(s = this.s + ToStringifier.stringify(that))

  def rplus(that: Any): MashString = copy(s = ToStringifier.stringify(that) + this.s)

  def reverse = copy(s = s.reverse)

  def first = copy(s = s.head + "")

  def last = copy(s = s.last + "")

  def length = MashNumber(s.length)

  def startsWith(that: MashString) = this.s.startsWith(that.s)

  def modify(f: String ⇒ String): MashString = copy(s = f(s))

  override def equals(that: Any) = cond(that) {
    case MashString(s2, _) ⇒ s == s2
  }

  def isEmpty = s.isEmpty

  override def hashCode = s.hashCode

  override def toString = s

  def compareTo(that: MashString) = this.s.compareTo(that.s)

}