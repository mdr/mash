package com.github.mdr.mash.runtime

import com.github.mdr.mash.GlobalInterpreterLock.withLock
import com.github.mdr.mash.evaluator.ToStringifier

import scala.collection.mutable.ArrayBuffer

object MashList {

  def apply(xs: Seq[_ <: MashValue]): MashList = new MashList(ArrayBuffer(xs: _*))

  def of(xs: MashValue*): MashList = new MashList(ArrayBuffer(xs: _*))

  def empty: MashList = of()

  def unapplySeq(x: MashList): Some[Seq[MashValue]] = Some(x.elements)

}

class MashList(val elements: ArrayBuffer[MashValue]) extends MashValue with Comparable[MashList] {

  def nonEmpty: Boolean = withLock { elements.nonEmpty }

  def isEmpty = withLock { elements.isEmpty }

  def head = withLock { elements.head }

  def last = withLock { elements.last }

  def init = withLock { elements.init }

  def forall(p: MashValue ⇒ Boolean): Boolean = withLock { elements.forall(p) }

  def foreach(f: MashValue ⇒ Unit): Unit = withLock { elements.foreach(f) }

  def size = withLock { elements.size }

  def length = withLock { size }

  def apply(i: Int): MashValue = withLock { elements(i) }

  def ++(that: MashList): MashList = withLock { new MashList(this.elements ++ that.elements) }

  def map(f: MashValue ⇒ MashValue): MashList = withLock { new MashList(this.elements.map(f)) }

  def take(n: Int) = withLock { new MashList(this.elements.take(n)) }

  def *(n: Int) = withLock { new MashList(ArrayBuffer((1 to n).flatMap(_ ⇒ elements): _*)) }

  def diff(that: MashList): MashList = withLock { new MashList(this.elements diff that.elements) }

  def intersect(that: MashList): MashList = withLock { new MashList(this.elements intersect that.elements) }

  override def equals(x: Any) = withLock {
    x match {
      case that: MashList ⇒ this.elements == that.elements
      case _              ⇒ false
    }
  }

  override def hashCode = withLock { this.elements.hashCode }

  override def toString = asString

  def asString = withLock {
    ToStringifier.visit(this, "[…]") {
      elements.mkString("[", ", ", "]")
    }
  }

  override def compareTo(that: MashList) = withLock { MashValueOrdering.compareLists(this.elements.toList, that.elements.toList) }

  def immutableElements: Seq[MashValue] = withLock { elements.toList }

}