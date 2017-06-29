package com.github.mdr.mash.runtime

import java.time.{ Instant, LocalDate }

import com.github.mdr.mash.classes.{ BoundMethod, MashClass }
import com.github.mdr.mash.functions.MashFunction
import com.github.mdr.mash.ns.collections.ListClass
import com.github.mdr.mash.ns.core._
import com.github.mdr.mash.ns.time.{ DateClass, DateTimeClass }

import scala.PartialFunction._

trait MashValue {

  def asObject: Option[MashObject] = condOpt(this) {
    case obj: MashObject ⇒ obj
  }

  def asList: Option[MashList] = condOpt(this) {
    case list: MashList ⇒ list
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
    case obj: MashObject                                                                  ⇒ obj.isEmpty
  }

  def isNull: Boolean = this == MashNull

  def isAString: Boolean = isA(StringClass)

  def isAnObject: Boolean = isA(ObjectClass)

  def isAList: Boolean = isA(ListClass)

  def isA(klass: MashClass): Boolean =
    (primaryClass isSubClassOf klass) || cond(this) {
      case taggable: TaggableMashValue ⇒ taggable.tagClassOpt.exists(_ isSubClassOf klass)
    }

}

trait TaggableMashValue extends MashValue {

  def tagClassOpt: Option[MashClass]

}