package com.github.mdr.mash.inference

import java.time.Instant
import java.time.LocalDate
import java.util.IdentityHashMap
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
import com.github.mdr.mash.runtime._
import com.github.mdr.mash.ns.core.TimedResultClass

object ValueTypeDetector {

  def getType(x: MashValue): Type = new ValueTypeDetector().getType(x)

}

/** Detect the type of runtime values **/
class ValueTypeDetector {

  private val visitingMap: IdentityHashMap[MashValue, Boolean] = new IdentityHashMap
  private val visitedMap: IdentityHashMap[MashValue, Type] = new IdentityHashMap

  def buildBindings(bindings: Map[String, MashValue]): Map[String, Type] =
    for ((k, v) ← bindings)
      yield k -> getType(v)

  def getType(x: MashValue): Type =
    Option(visitedMap.get(x)).getOrElse {
      if (visitingMap containsKey x)
        Type.Any
      else {
        visitingMap.put(x, true)
        try {
          val typ_ = getType_(x)
          visitedMap.put(x, typ_)
          typ_
        } finally
          visitingMap.remove(x)
      }
    }

  def getType_(x: MashValue): Type = x match {
    case MashNull                                ⇒ Type.Instance(NullClass)
    case AnonymousFunction(param, body, context) ⇒ Type.Lambda(param.params.map(_.name), body, buildBindings(context.scopeStack.bindings))
    case f: MashFunction                         ⇒ Type.DefinedFunction(f)
    case BoundMethod(target, method, _)          ⇒ Type.BoundMethod(getType(target), method)
    case MashString(_, None)                     ⇒ Type.Instance(StringClass)
    case MashString(_, Some(tagClass))           ⇒ StringClass taggedWith tagClass
    case MashNumber(_, None)                     ⇒ Type.Instance(NumberClass)
    case MashNumber(_, Some(tagClass))           ⇒ NumberClass taggedWith tagClass
    case _: MashBoolean                          ⇒ Type.Instance(BooleanClass)
    case MashWrapped(_: Instant)                 ⇒ Type.Instance(DateTimeClass)
    case MashWrapped(_: LocalDate)               ⇒ Type.Instance(LocalDateClass)
    case _: MashClass                            ⇒ Type.Instance(ClassClass)
    case MashUnit                                ⇒ Type.Instance(UnitClass)
    case xs: MashList                            ⇒ xs.items.headOption.map(item ⇒ Type.Seq(getType(item))).getOrElse(Type.Seq(Type.Any))
    case obj @ MashObject(_, None)               ⇒ Type.Object(for ((field, value) ← obj.immutableFields) yield field -> getType(value))
    case obj @ MashObject(_, Some(GroupClass))   ⇒ getTypeOfGroup(obj)
    case obj @ MashObject(_, Some(TimedResultClass)) ⇒
      (for {
        key ← obj.get(TimedResultClass.Fields.Result)
        keyType = getType(key)
      } yield Type.TimedResult(keyType)) getOrElse Type.TimedResult(Type.Any)
    case MashObject(_, Some(klass)) ⇒ Type.Instance(klass)
    case _                          ⇒ Type.Any
  }

  private def getTypeOfGroup(obj: MashObject) = {
    val groupTypeOpt =
      for {
        key ← obj.get(GroupClass.Fields.Key)
        keyType = getType(key)
        values ← obj.get(GroupClass.Fields.Values)
        valuesType = getType(values)
      } yield valuesType match {
        case Type.Seq(valueType) ⇒ Type.Group(keyType, valueType)
        case _                   ⇒ Type.Group(keyType, Type.Any)
      }
    groupTypeOpt.getOrElse(Type.Group(Type.Any, Type.Any))
  }

}