package com.github.mdr.mash.runtime

import com.github.mdr.mash.GlobalInterpreterLock
import com.github.mdr.mash.GlobalInterpreterLock.withLock
import com.github.mdr.mash.evaluator.ToStringifier

import scala.collection.mutable.ArrayBuffer

object MashList {

  def apply(xs: Seq[_ <: MashValue]): MashList = new MashList(ArrayBuffer(xs: _*))

  def of(xs: MashValue*): MashList = new MashList(ArrayBuffer(xs: _*))

  def empty: MashList = of()

  def unapplySeq(x: MashList): Some[Seq[MashValue]] = Some(x.items)

}

class MashList(val items: ArrayBuffer[MashValue]) extends MashValue with Comparable[MashList] {

  def nonEmpty: Boolean = withLock { items.nonEmpty }

  def isEmpty = withLock { items.isEmpty }

  def head = withLock { items.head }

  def last = withLock { items.last }

  def forall(p: MashValue ⇒ Boolean): Boolean = withLock { items.forall(p) }

  def foreach(f: MashValue ⇒ Unit): Unit = withLock { items.foreach(f) }

  def size = withLock { items.size }

  def length = withLock { size }

  def apply(i: Int): MashValue = withLock { items(i) }

  def ++(that: MashList): MashList = withLock { new MashList(this.items ++ that.items) }

  def map(f: MashValue ⇒ MashValue): MashList = withLock { new MashList(this.items.map(f)) }

  def take(n: Int) = withLock { new MashList(this.items.take(n)) }

  def *(n: Int) = withLock { new MashList(ArrayBuffer((1 to n).flatMap(_ ⇒ items): _*)) }

  override def equals(x: Any) = withLock {
    x match {
      case that: MashList ⇒ this.items == that.items
      case _              ⇒ false
    }
  }

  override def hashCode = withLock { this.items.hashCode }

  override def toString = asString

  def asString = withLock {
    ToStringifier.visit(this, "[…]") {
      items.mkString("[", ", ", "]")
    }
  }

  override def compareTo(that: MashList) = withLock { MashValueOrdering.compare(this.items.toList, that.items.toList) }

}