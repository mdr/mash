package com.github.mdr.mash.runtime

import java.time.{ Instant, LocalDate, ZoneId }

import com.github.mdr.mash.classes.MashClass
import com.github.mdr.mash.ns.collections.ListClass
import com.github.mdr.mash.ns.core._
import com.github.mdr.mash.ns.time.{ DateClass, DateTimeClass }

import scala.collection.immutable.ListMap

object MashClassOrdering extends Ordering[MashClass] {

  private val specialClassPosition: Map[MashClass, Int] =
    Map(
      NullClass -> 0,
      UnitClass -> 1,
      BooleanClass -> 2,
      NumberClass -> 3,
      StringClass -> 4,
      DateClass -> 5,
      DateTimeClass -> 6,
      FunctionClass -> 7,
      BoundMethodClass -> 8,
      ListClass -> 9,
      ObjectClass -> 10,
      ClassClass -> 11,
      AnyClass -> 12)

  override def compare(class1: MashClass, class2: MashClass): Int =
    (specialClassPosition.get(class1), specialClassPosition.get(class2)) match {
      case (Some(n1), Some(n2)) ⇒ n1 compareTo n2
      case (Some(n1), None)     ⇒ -1
      case (None, Some(n2))     ⇒ 1
      case (None, None)         ⇒ class1.fullyQualifiedName.toString compareTo class2.fullyQualifiedName.toString
    }

}

object MashValueOrdering extends Ordering[MashValue] {

  override def compare(v1: MashValue, v2: MashValue): Int = (v1, v2) match {
    case (c1: MashClass, c2: MashClass)                           ⇒ c1 compareTo c2
    case (b1: MashBoolean, b2: MashBoolean)                       ⇒ b1 compareTo b2
    case (n1: MashNumber, n2: MashNumber)                         ⇒ n1 compareTo n2
    case (s1: MashString, s2: MashString)                         ⇒ s1 compareTo s2
    case (MashWrapped(d1: LocalDate), MashWrapped(d2: LocalDate)) ⇒ d1 compareTo d2
    case (MashWrapped(t1: Instant), MashWrapped(t2: Instant))     ⇒ t1 compareTo t2
    case (MashWrapped(d1: LocalDate), MashWrapped(t2: Instant))   ⇒ toInstant(d1) compareTo t2
    case (MashWrapped(t1: Instant), MashWrapped(d2: LocalDate))   ⇒ t1 compareTo toInstant(d2)
    case (list1: MashList, list2: MashList)                       ⇒ list1 compareTo list2
    case (obj1: MashObject, obj2: MashObject)                     ⇒ obj1 compareTo obj2
    case (_, _)                                                   ⇒ MashClassOrdering.compare(v1.primaryClass, v2.primaryClass)
  }

  private def toInstant(date: LocalDate): Instant = date.atStartOfDay(ZoneId.systemDefault).toInstant

  def compare(xs: List[MashValue], ys: List[MashValue]): Int = (xs, ys) match {
    case (Nil, Nil)               ⇒ 0
    case (_, Nil)                 ⇒ 1
    case (Nil, _)                 ⇒ -1
    case (x :: tailX, y :: tailY) ⇒
      val c = compare(x, y)
      if (c == 0)
        compare(tailX, tailY)
      else
        c
  }

  def compare(xs: ListMap[String, MashValue], ys: ListMap[String, MashValue]): Int = {
    def compareP(xs: List[(String, MashValue)], ys: List[(String, MashValue)]): Int = (xs, ys) match {
      case (Nil, Nil)                             ⇒ 0
      case (_, Nil)                               ⇒ 1
      case (Nil, _)                               ⇒ -1
      case ((s1, v1) :: tailX, (s2, v2) :: tailY) ⇒
        val c = s1 compareTo s2
        if (c == 0) {
          val c2 = compare(v1, v2)
          if (c2 == 0)
            compareP(tailX, tailY)
          else
            c2
        } else
          c
    }
    compareP(xs.toList, ys.toList)
  }

}
