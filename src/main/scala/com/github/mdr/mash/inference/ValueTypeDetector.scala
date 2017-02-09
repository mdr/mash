package com.github.mdr.mash.inference

import java.time.{ Instant, LocalDate }
import java.util.IdentityHashMap

import com.github.mdr.mash.classes.MashClass
import com.github.mdr.mash.evaluator.BoundMethod
import com.github.mdr.mash.functions.{ UserDefinedClass, _ }
import com.github.mdr.mash.ns.collections.GroupClass
import com.github.mdr.mash.ns.core._
import com.github.mdr.mash.ns.time.{ DateClass, DateTimeClass }
import com.github.mdr.mash.runtime._

import scala.collection.immutable.ListMap

object ValueTypeDetector {

  def getType(x: MashValue): Type = new ValueTypeDetector().getType(x)

  private val visitedMap: IdentityHashMap[MashValue, Type] = new IdentityHashMap

}

/** Detect the type of runtime values **/
class ValueTypeDetector {

  import ValueTypeDetector._

  def buildBindings(bindings: Map[String, MashValue]): Map[String, Type] =
    for ((k, v) ← bindings)
      yield k -> getType(v)

  private val visitingMap: IdentityHashMap[MashValue, Boolean] = new IdentityHashMap

  def getType(x: MashValue): Type = ValueTypeDetector.synchronized {
    Option(visitedMap.get(x)).getOrElse {
      if (visitingMap containsKey x)
        Type.Any
      else {
        visitingMap.put(x, true)
        try {
          val type_ = getType_(x)
          visitedMap.put(x, type_)
          type_
        } finally
          visitingMap.remove(x)
      }
    }
  }

  def getType_(x: MashValue): Type = x match {
    case MashNull                                                                ⇒ NullClass
    case AnonymousFunction(parameterModel, body, context)                        ⇒ Type.UserDefinedFunction(docCommentOpt = None, isPrivate = false, None, parameterModel, body, buildBindings(context.scopeStack.bindings))
    case UserDefinedFunction(docCommentOpt, name, parameterModel, body, context) ⇒ Type.UserDefinedFunction(docCommentOpt, isPrivate = false, Some(name), parameterModel, body, buildBindings(context.scopeStack.bindings))
    case f: MashFunction                                                         ⇒ Type.BuiltinFunction(f)
    case BoundMethod(target, method: UserDefinedMethod, _)                       ⇒ getBoundMethodType(target, method)
    case BoundMethod(target, method, _)                                          ⇒ Type.BoundBuiltinMethod(getType(target), method)
    case MashString(_, None)                                                     ⇒ StringClass
    case MashString(_, Some(tagClass))                                           ⇒ StringClass taggedWith tagClass
    case MashNumber(_, None)                                                     ⇒ NumberClass
    case MashNumber(_, Some(tagClass))                                           ⇒ NumberClass taggedWith tagClass
    case _: MashBoolean                                                          ⇒ BooleanClass
    case MashWrapped(_: Instant)                                                 ⇒ DateTimeClass
    case MashWrapped(_: LocalDate)                                               ⇒ DateClass
    case UserDefinedClass(_, name, _, params, methods)                           ⇒ Type.UserClass(name, params, getMethodTypes(methods))
    case _: MashClass                                                            ⇒ ClassClass
    case MashUnit                                                                ⇒ Unit
    case xs: MashList                                                            ⇒ xs.elements.headOption.map(getType).getOrElse(Type.Any).seq
    case obj@MashObject(_, None)                                                 ⇒ Type.Object(for ((field, value) ← obj.immutableFields) yield field -> getType(value))
    case obj@MashObject(_, Some(GroupClass))                                     ⇒ getTypeOfGroup(obj)
    case obj@MashObject(_, Some(TimedResultClass))                               ⇒ getTypeOfTimedResult(obj)
    case MashObject(_, Some(UserDefinedClass(_, name, _, params, methods)))      ⇒ Type.UserClassInstance(Type.UserClass(name, params, getMethodTypes(methods)))
    case MashObject(_, Some(klass))                                              ⇒ klass
    case _                                                                       ⇒ Type.Any
  }

  private def getBoundMethodType(target: MashValue, method: UserDefinedMethod) = {
    val UserDefinedMethod(docCommentOpt, name, params, _, body, context, isPrivate, _) = method
    val bindings = buildBindings(context.scopeStack.bindings)
    val methodType = Type.UserDefinedFunction(docCommentOpt, isPrivate, Some(name), params, body, bindings)
    Type.BoundUserDefinedMethod(getType(target), methodType)
  }

  def instanceType(userDefinedClass: UserDefinedClass): Type.UserClassInstance = {
    val UserDefinedClass(_, name, _, params, methods) = userDefinedClass
    Type.UserClassInstance(Type.UserClass(name, params, getMethodTypes(methods)))
  }

  def getMethodTypes(methods: Seq[UserDefinedMethod]): ListMap[String, Type.UserDefinedFunction] = {
    val pairs = methods.map { method ⇒
      val bindings = buildBindings(method.context.scopeStack.bindings)
      val functionType = Type.UserDefinedFunction(method.docCommentOpt, method.isPrivate, Some(method.name),
        method.params, method.body, bindings)
      method.name -> functionType
    }
    ListMap(pairs: _*)
  }

  private def getTypeOfTimedResult(obj: MashObject): Type = {
    val resultType = obj.get(TimedResultClass.Fields.Result).map(getType).getOrElse(Type.Any)
    TimedResultClass.withGenerics(resultType)
  }

  private def getTypeOfGroup(obj: MashObject): Type = {
    val groupTypeOpt =
      for {
        key ← obj.get(GroupClass.Fields.Key)
        keyType = getType(key)
        values ← obj.get(GroupClass.Fields.Values)
        valueType = getType(values) match {
          case Type.Seq(valueType) ⇒ valueType
          case _                   ⇒ Type.Any
        }
      } yield GroupClass.withGenerics(keyType, valueType)
    groupTypeOpt.getOrElse(GroupClass.withGenerics(Type.Any, Type.Any))
  }

}