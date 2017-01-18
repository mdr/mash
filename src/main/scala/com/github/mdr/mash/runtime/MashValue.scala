package com.github.mdr.mash.runtime

import java.time.{ Instant, LocalDate, ZoneId, ZoneOffset }

import com.github.mdr.mash.evaluator.{ BoundMethod, EvaluatorException, MashClass, SourceLocation }
import com.github.mdr.mash.functions.MashFunction
import com.github.mdr.mash.ns.collections.ListClass
import com.github.mdr.mash.ns.core._
import com.github.mdr.mash.ns.time.{ DateClass, DateTimeClass }

import scala.PartialFunction._

trait MashValue {

  def asObject: Option[MashObject] = condOpt(this) {
    case obj: MashObject ⇒ obj
  }

  def typeName: String = primaryClass.name

  def primaryClass: MashClass = this match {
    case MashNull                  ⇒ NullClass
    case MashUnit                  ⇒ UnitClass
    case obj: MashObject           ⇒ obj.classOpt getOrElse ObjectClass
    case _: MashNumber             ⇒ NumberClass
    case _: MashString             ⇒ StringClass
    case _: MashBoolean            ⇒ BooleanClass
    case _: MashList               ⇒ ListClass
    case MashWrapped(_: Instant)   ⇒ DateTimeClass
    case MashWrapped(_: LocalDate) ⇒ DateClass
    case _: MashFunction           ⇒ FunctionClass
    case _: BoundMethod            ⇒ BoundMethodClass
    case _: MashClass              ⇒ ClassClass
  }

  def isTruthy: Boolean = !isFalsey

  def isFalsey: Boolean = cond(this) {
    case MashBoolean.False | MashNull | MashNumber(0, _) | MashString("", _) | MashList() ⇒ true
    case MashObject(fields, None)                                                         ⇒ fields.isEmpty
  }

  def isNull: Boolean = this == MashNull

  def isAString: Boolean = isA(StringClass)

  def isAnObject: Boolean = isA(ObjectClass)

  def isA(klass: MashClass): Boolean =
    (primaryClass isSubClassOf klass) || cond(this) {
      case taggable: TaggableMashValue ⇒ taggable.tagClassOpt.exists(_ isSubClassOf klass)
    }

}

object MashValueOrdering extends Ordering[MashValue] {

  override def compare(v1: MashValue, v2: MashValue): Int = compareWithLocation(v1, v2)

  def compareWithLocation(v1: MashValue, v2: MashValue, locationOpt: Option[SourceLocation] = None): Int =
    compareOpt(v1, v2).getOrElse(
      throw new EvaluatorException(s"Incomparable values of type ${v1.typeName} and ${v2.typeName}", locationOpt))

  def compareOpt(v1: MashValue, v2: MashValue): Option[Int] =
    condOpt((v1, v2)) {
      case (b1: MashBoolean, b2: MashBoolean)                       ⇒ b1 compareTo b2
      case (n1: MashNumber, n2: MashNumber)                         ⇒ n1 compareTo n2
      case (s1: MashString, s2: MashString)                         ⇒ s1 compareTo s2
      case (MashWrapped(d1: LocalDate), MashWrapped(d2: LocalDate)) ⇒ d1 compareTo d2
      case (MashWrapped(t1: Instant), MashWrapped(t2: Instant))     ⇒ t1 compareTo t2
      case (MashWrapped(d1: LocalDate), MashWrapped(t2: Instant))   ⇒ d1.atStartOfDay(ZoneId.systemDefault).toInstant compareTo t2
      case (MashWrapped(t1: Instant), MashWrapped(d2: LocalDate))   ⇒ t1 compareTo d2.atStartOfDay(ZoneId.systemDefault).toInstant
      case (list1: MashList, list2: MashList)                       ⇒ list1 compareTo list2
    }

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

}

trait TaggableMashValue extends MashValue {

  def tagClassOpt: Option[MashClass]

}