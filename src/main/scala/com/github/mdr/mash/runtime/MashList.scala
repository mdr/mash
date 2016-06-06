package com.github.mdr.mash.runtime

import scala.collection.mutable.ArrayBuffer

object MashList {

  def apply(xs: Seq[_ <: MashValue]): MashList = new MashList(ArrayBuffer(xs: _*))

  def of(xs: MashValue*): MashList = new MashList(ArrayBuffer(xs: _*))

  def unapplySeq(x: MashList): Some[Seq[MashValue]] = Some(x.items)

}

class MashList(val items: ArrayBuffer[MashValue]) extends MashValue {

  def nonEmpty: Boolean = items.nonEmpty

  def isEmpty = items.isEmpty

  def head = items.head

  def last = items.last

  def forall(p: MashValue ⇒ Boolean): Boolean = items.forall(p)

  def foreach(f: MashValue ⇒ Unit): Unit = items.foreach(f)

  def size = items.size

  def length = size

  def apply(i: Int): MashValue = items(i)

  def ++(that: MashList): MashList = new MashList(this.items ++ that.items)

  def map(f: MashValue ⇒ MashValue): MashList = new MashList(this.items.map(f))

  def take(n: Int) = new MashList(this.items.take(n))

  override def equals(x: Any) = x match {
    case that: MashList ⇒ this.items == that.items
    case _              ⇒ false
  }

  override def toString = items.mkString("[", ", ", "]")

  override def hashCode = this.items.hashCode
}