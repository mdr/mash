package com.github.mdr.mash.inference

import java.time.Instant
import java.time.LocalDate
import com.github.mdr.mash.evaluator.BoundMethod
import com.github.mdr.mash.evaluator.MashClass
import com.github.mdr.mash.functions.AnonymousFunction
import com.github.mdr.mash.functions.MashFunction
import com.github.mdr.mash.ns.collections.GroupClass
import com.github.mdr.mash.ns.core.BooleanClass
import com.github.mdr.mash.ns.core.ClassClass
import com.github.mdr.mash.ns.core.NullClass
import com.github.mdr.mash.ns.core.NumberClass
import com.github.mdr.mash.ns.core.StringClass
import com.github.mdr.mash.ns.core.UnitClass
import com.github.mdr.mash.ns.time.DateTimeClass
import com.github.mdr.mash.ns.time.LocalDateClass
import com.github.mdr.mash.runtime.MashBoolean
import com.github.mdr.mash.runtime.MashList
import com.github.mdr.mash.runtime.MashNull
import com.github.mdr.mash.runtime.MashNumber
import com.github.mdr.mash.runtime.MashObject
import com.github.mdr.mash.runtime.MashString
import com.github.mdr.mash.runtime.MashUnit
import com.github.mdr.mash.runtime.MashWrapped
import com.github.mdr.mash.runtime.MashValue
import java.util.IdentityHashMap

object ValueTypeDetector {

  def getType(x: MashValue): Type = new ValueTypeDetector().getType(x)

}

/** Detect the type of runtime values **/
class ValueTypeDetector {

  private val visitedMap: IdentityHashMap[MashValue, Boolean] = new IdentityHashMap

  def buildBindings(bindings: Map[String, MashValue], includeGlobal: Boolean = true): Map[String, Type] =
    for ((k, v) ← bindings)
      yield k -> getType(v)

  def getType(x: MashValue): Type =
    if (visitedMap containsKey x)
      Type.Any
    else {
      visitedMap.put(x, true)
      try
        getType_(x)
      finally
        visitedMap.remove(x)
    }

  def getType_(x: MashValue): Type = x match {
    case MashNull                            ⇒ Type.Instance(NullClass)
    case AnonymousFunction(param, body, ctx) ⇒ Type.Lambda(param, body, buildBindings(ctx.scopeStack.bindings, includeGlobal = false))
    case f: MashFunction                     ⇒ Type.DefinedFunction(f)
    case BoundMethod(target, method, _)      ⇒ Type.BoundMethod(getType(target), method)
    case MashString(_, None)                 ⇒ Type.Instance(StringClass)
    case MashString(_, Some(tagClass))       ⇒ Type.Tagged(StringClass, tagClass)
    case MashNumber(_, None)                 ⇒ Type.Instance(NumberClass)
    case MashNumber(_, Some(tagClass))       ⇒ Type.Tagged(NumberClass, tagClass)
    case _: MashBoolean                      ⇒ Type.Instance(BooleanClass)
    case MashWrapped(_: Instant)             ⇒ Type.Instance(DateTimeClass)
    case MashWrapped(_: LocalDate)           ⇒ Type.Instance(LocalDateClass)
    case _: MashClass                        ⇒ Type.Instance(ClassClass)
    case MashUnit                            ⇒ Type.Instance(UnitClass)
    case mo @ MashObject(_, Some(GroupClass)) ⇒
      val groupTypeOpt =
        for {
          key ← mo.getField(GroupClass.Fields.Key)
          keyType = getType(key)
          values ← mo.getField(GroupClass.Fields.Values)
          valuesType = getType(values)
        } yield valuesType match {
          case Type.Seq(valueType) ⇒ Type.Group(keyType, valueType)
          case _                   ⇒ Type.Group(keyType, Type.Any)
        }
      groupTypeOpt.getOrElse(Type.Group(Type.Any, Type.Any))
    case MashObject(_, Some(klass)) ⇒ Type.Instance(klass)
    case obj @ MashObject(_, None)  ⇒ Type.Object(for ((field, value) ← obj.immutableFields) yield field -> getType(value))
    case xs: MashList ⇒
      val sequenceTypes = xs.items.map(getType).distinct
      sequenceTypes match {
        case Seq(sequenceType) ⇒ Type.Seq(sequenceType)
        case _                 ⇒ Type.Seq(Type.Any)
      }
    case _ ⇒ Type.Any
  }

}