package com.github.mdr.mash.utils

import scala.PartialFunction.cond
import scala.collection.mutable.ArrayBuffer

object Utils {

  def adjacentPairs[T](xs: Seq[T]): Seq[(T, T)] = xs.zip(xs.tail)

  def time[T](label: String)(p: ⇒ T) = {
    val start = System.nanoTime
    try
      p
    finally {
      val duration = (System.nanoTime - start) / 1000000.0
      println(s"$label: $duration ms")
    }
  }

  def max[A](xs: Iterable[A])(implicit cmp: Ordering[A]): Option[A] =
    if (xs.isEmpty) None else Some(xs.max)

  def max[A](xs: Iterable[A], default: A)(implicit cmp: Ordering[A]): A = max(xs) getOrElse default

  def minBy[A, B](xs: Seq[A], f: A ⇒ B)(implicit cmp: Ordering[B]): Option[A] =
    if (xs.isEmpty) None else Some(xs.minBy(f))

  def maxBy[A, B](xs: Seq[A], f: A ⇒ B)(implicit cmp: Ordering[B]): Option[A] =
    if (xs.isEmpty) None else Some(xs.maxBy(f))

  def maxBy[A, B](xs: Seq[A], f: A ⇒ B, default: A)(implicit cmp: Ordering[B]): A =
    maxBy(xs, f) getOrElse default

  def itemBefore[T](items: Seq[T], item: T): Option[T] =
    Utils.indexOf(items, item)
      .collect { case n if n > 0 ⇒ n - 1 }
      .flatMap(items.lift)

  def indexOf[T](xs: Seq[T], item: T): Option[Int] = xs.indexOf(item) match {
    case -1 ⇒ None
    case n  ⇒ Some(n)
  }

  def indexOf(s: String, s2: String): Option[Int] = s.indexOf(s2) match {
    case -1 ⇒ None
    case n  ⇒ Some(n)
  }

  def indexWhere[T](xs: Seq[T], f: T ⇒ Boolean): Option[Int] = xs.indexWhere(f) match { case -1 ⇒ None; case i ⇒ Some(i) }

  def sequence[T](xs: Seq[Option[T]]): Option[Seq[T]] = {
    val result = ArrayBuffer[T]()
    xs.foreach {
      case Some(x) ⇒ result += x
      case None    ⇒ return None
    }
    Some(result)
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
    case class Wrapper(item: T) {
      val key: U = f(item)
      override def hashCode = key.hashCode
      override def equals(other: Any) = cond(other) {
        case that @ Wrapper(_) ⇒ this.key == that.key
      }
    }
    items.map(Wrapper).distinct.map(_.item)
  }

  def optionCombine[T](opt1: Option[T], opt2: Option[T], f: (T, T) ⇒ T): Option[T] =
    (opt1, opt2) match {
      case (_, None)                    ⇒ opt1
      case (None, _)                    ⇒ opt2
      case (Some(value1), Some(value2)) ⇒ Some(f(value1, value2))
    }

  def tupled[A, B, C](f: (A, B) ⇒ C): ((A, B)) ⇒ C = f.tupled

  implicit class RichWhen[T](t: T) {

    def when(cond: Boolean, f: T ⇒ T): T = if (cond) f(t) else t

  }


}