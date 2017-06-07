package com.github.mdr.mash.runtime

import com.github.mdr.mash.classes.MashClass
import com.github.mdr.mash.evaluator.ToStringifier
import com.github.mdr.mash.ns.core.CharacterClass

import scala.PartialFunction.cond

object MashString {

  def apply(s: String, tagClass: MashClass): MashString = MashString(s, Some(tagClass))

}

case class MashString(s: String, tagClassOpt: Option[MashClass] = None) extends TaggableMashValue with Comparable[MashString] {
  assert(s != null, "string cannot be null")
  
  def lookup(i: Int) = {
    val index = if (i < 0) i + s.size else i
    MashString(s(index).toString, CharacterClass)
  }

  def +(that: MashValue): MashString = copy(s = this.s + ToStringifier.stringify(that))

  def *(n: Int): MashString = modify(_ * n)
  
  def rplus(that: MashValue): MashString = copy(s = ToStringifier.stringify(that) + this.s)

  def reverse = copy(s = s.reverse)

  def first = copy(s = s.head + "")

  def last = copy(s = s.last + "")

  def init = copy(s = s.init)

  def length = s.length

  def startsWith(that: MashString) = this.s.startsWith(that.s)

  def modify(f: String ⇒ String): MashString = copy(s = f(s))

  override def equals(that: Any) = cond(that) {
    case MashString(s2, _) ⇒ s == s2
  }

  def isEmpty = s.isEmpty

  override def hashCode = s.hashCode

  override def toString = s

  def compareTo(that: MashString) = this.s.compareTo(that.s)

  def withTag(tagClass: MashClass): MashString = copy(tagClassOpt = Some(tagClass))

}