package com.github.mdr.mash.evaluator

import java.time.Instant
import com.github.mdr.mash.functions.MashFunction
import com.github.mdr.mash.ns.collections.SeqClass
import com.github.mdr.mash.ns.core._
import com.github.mdr.mash.ns.time.DateTimeClass
import com.github.mdr.mash.parser.AbstractSyntax._
import com.github.mdr.mash.utils.PointedRegion
import java.time.LocalDate
import com.github.mdr.mash.ns.time.LocalDateClass
import com.github.mdr.mash.runtime.MashObject
import com.github.mdr.mash.runtime.MashString
import com.github.mdr.mash.runtime.MashNumber
import com.github.mdr.mash.runtime.MashList
import com.github.mdr.mash.runtime.MashNull
import com.github.mdr.mash.runtime.MashBoolean
import com.github.mdr.mash.runtime.MashUnit
import com.github.mdr.mash.runtime.MashWrapped

object MemberEvaluator {

  private def lookupMethod(target: Any, klass: MashClass, name: String): Option[Any] = {
    val directResultOpt =
      for {
        method ← klass.getMethod(name)
      } yield BoundMethod(target, method, klass)
    lazy val parentResultOpt = klass.parentOpt.flatMap(parentClass ⇒ lookupMethod(target, parentClass, name))
    directResultOpt orElse parentResultOpt
  }

  private def lookupMethod(target: MashObject, name: String): Option[Any] =
    for {
      klass ← target.classOpt orElse Some(ObjectClass)
      boundMethod ← lookupMethod(target, klass, name)
    } yield boundMethod

  def lookup(target: Any, field: Field): Any =
    lookup(target, field.name)

  def lookup(target: Any, name: String, locationOpt: Option[PointedRegion] = None): Any =
    maybeLookup(target, name).getOrElse(
      throw new EvaluatorException(s"Cannot find member '$name' in $target", locationOpt))

  def hasMember(target: Any, name: String): Boolean =
    maybeLookup(target, name).isDefined

  def maybeLookup(target: Any, name: String): Option[Any] =
    target match {
      case MashNumber(n, tagClassOpt)   ⇒ lookupMethod(target, NumberClass, name) orElse tagClassOpt.flatMap(tag ⇒ lookupMethod(target, tag, name))
      case MashString(s, tagClassOpt)   ⇒ lookupMethod(target, StringClass, name) orElse tagClassOpt.flatMap(tag ⇒ lookupMethod(target, tag, name))
      case MashNull                     ⇒ lookupMethod(target, NullClass, name)
      case MashUnit                     ⇒ lookupMethod(target, UnitClass, name)
      case b: MashBoolean               ⇒ lookupMethod(b, BooleanClass, name)
      case xs: MashList                 ⇒ lookupMethod(xs, SeqClass, name)
      case obj: MashObject              ⇒ obj.getField(name) orElse lookupMethod(obj, name)
      case f: MashFunction              ⇒ lookupMethod(f, FunctionClass, name)
      case bm: BoundMethod              ⇒ lookupMethod(bm, BoundMethodClass, name)
      case klass: MashClass             ⇒ lookupMethod(klass, ClassClass, name)
      case MashWrapped(dt: Instant)     ⇒ lookupMethod(dt, DateTimeClass, name)
      case MashWrapped(date: LocalDate) ⇒ lookupMethod(date, LocalDateClass, name)
      case _                            ⇒ None
    }

}