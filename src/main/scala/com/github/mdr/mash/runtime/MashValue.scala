package com.github.mdr.mash.runtime

import java.time.Instant
import java.time.LocalDate
import scala.PartialFunction._
import com.github.mdr.mash.evaluator.BoundMethod
import com.github.mdr.mash.evaluator.EvaluatorException
import com.github.mdr.mash.evaluator.MashClass
import com.github.mdr.mash.functions.MashFunction
import com.github.mdr.mash.ns.collections.ListClass
import com.github.mdr.mash.ns.core._
import com.github.mdr.mash.ns.time.DateTimeClass
import com.github.mdr.mash.ns.time.LocalDateClass
import com.github.mdr.mash.evaluator.SourceLocation

trait MashValue {

  def asObject: Option[MashObject] = condOpt(this) {
    case obj: MashObject ⇒ obj
  }

  def typeName: String = primaryClass.name

  def primaryClass: MashClass = this match {
    case MashNull                  ⇒ NullClass
    case MashUnit                  ⇒ UnitClass
    case obj: MashObject           ⇒ obj.classOpt.getOrElse(ObjectClass)
    case _: MashNumber             ⇒ NumberClass
    case _: MashString             ⇒ StringClass
    case _: MashBoolean            ⇒ BooleanClass
    case _: MashList               ⇒ ListClass
    case MashWrapped(_: Instant)   ⇒ DateTimeClass
    case MashWrapped(_: LocalDate) ⇒ LocalDateClass
    case _: MashFunction           ⇒ FunctionClass
    case _: BoundMethod            ⇒ BoundMethodClass
    case _: MashClass              ⇒ ClassClass
  }

  def isTruthy: Boolean = !isFalsey

  def isFalsey: Boolean = cond(this) {
    case MashBoolean.False | MashNull | MashNumber(0, _) | MashString("", _) | MashList() ⇒ true
    case MashObject(fields, None) ⇒ fields.isEmpty
  }

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
      case (n1: MashNumber, n2: MashNumber)                         ⇒ n1 compareTo n2
      case (s1: MashString, s2: MashString)                         ⇒ s1 compareTo s2
      case (MashWrapped(d1: LocalDate), MashWrapped(d2: LocalDate)) ⇒ d1 compareTo d2
      case (MashWrapped(t1: Instant), MashWrapped(t2: Instant))     ⇒ t1 compareTo t2
    }

}

trait TaggableMashValue extends MashValue {

  def tagClassOpt: Option[MashClass]

}