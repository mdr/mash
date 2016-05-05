package com.github.mdr.mash.utils

import scala.collection.mutable.ArrayBuffer
import scala.PartialFunction.cond

object Utils {

  def indexOf(s: String, s2: String): Option[Int] = s.indexOf(s2) match {
    case -1 ⇒ None
    case n  ⇒ Some(n)
  }

  def indexWhere[T](xs: Seq[T], f: T => Boolean): Option[Int] = xs.indexWhere(f) match { case -1 => None; case i => Some(i) }
  
  def sequence[T](xs: Seq[Option[T]]): Option[Seq[T]] = {
    val result = ArrayBuffer[T]()
    xs.foreach(
      _ match {
        case Some(x) ⇒ result += x
        case None    ⇒ return None
      })
    Some(result)
  }

  val AnyOrdering: Ordering[Any] = new Ordering[Any] {
    def compare(x: Any, y: Any): Int = x.asInstanceOf[Comparable[Any]].compareTo(y)
  }

  /**
   * Zip two sequences together, but instead of truncating to the shortest of the two if the lengths are unequal, extend
   * using the given padding value.
   */
  def zipPad[T](xs: Seq[T], ys: Seq[T], padding: T): Seq[(T, T)] =
    if (xs.length > ys.length)
      xs.zip(ys ++ Seq.fill(xs.length - ys.length)(padding))
    else
      (xs ++ Seq.fill(ys.length - xs.length)(padding)).zip(ys)

  def commonPrefix[T](xs: Seq[T], ys: Seq[T]): Seq[T] = (xs, ys) match {
    case (Seq(x, xsRest @ _*), Seq(y, ysRest @ _*)) if x == y ⇒ x +: commonPrefix(xsRest, ysRest)
    case _ ⇒ Nil
  }

  def initOpt[T](xs: Seq[T]): Option[Seq[T]] = if (xs.isEmpty) None else Some(xs.init)

  def intercalate[T](xss: Seq[Seq[T]], xs: Seq[T]): Seq[T] = xss match {
    case Seq()    ⇒ Seq()
    case Seq(xs_) ⇒ xs_
    case _        ⇒ xss.head ++ xs ++ intercalate(xss.tail, xs)
  }

  def distinctBy[T, U](items: Seq[T], f: T ⇒ U): Seq[T] = {
    case class Wrapper(val item: T) {
      val key: U = f(item)
      override def hashCode = key.hashCode
      override def equals(other: Any) = cond(other) {
        case that @ Wrapper(_) ⇒ this.key == that.key
      }
    }
    items.map(Wrapper).distinct.map(_.item)
  }

}