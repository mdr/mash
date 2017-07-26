package com.github.mdr.mash.inference

import java.time.{ Instant, LocalDate }
import java.util.IdentityHashMap

import com.github.mdr.mash.classes._
import com.github.mdr.mash.functions._
import com.github.mdr.mash.ns.collections.GroupClass
import com.github.mdr.mash.ns.core._
import com.github.mdr.mash.ns.time.{ DateClass, DateTimeClass }
import com.github.mdr.mash.runtime._

import scala.collection.immutable.ListMap
import scala.collection.mutable.ArrayBuffer

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

  private def getType_(x: MashValue): Type = x match {
    case MashNull                                          ⇒ NullClass
    case f: AnonymousFunction                              ⇒ getUserFunctionType(f)
    case f: UserDefinedFunction                            ⇒ getUserFunctionType(f)
    case f: MashFunction                                   ⇒ Type.BuiltinFunction(f)
    case BoundMethod(target, method: UserDefinedMethod, _) ⇒ getBoundMethodType(target, method)
    case BoundMethod(target, method, _)                    ⇒ Type.BoundBuiltinMethod(getType(target), method)
    case MashString(_, None)                               ⇒ StringClass
    case MashString(_, Some(tagClass))                     ⇒ StringClass taggedWith tagClass
    case MashNumber(_, None)                               ⇒ NumberClass
    case MashNumber(_, Some(tagClass))                     ⇒ NumberClass taggedWith tagClass
    case _: MashBoolean                                    ⇒ BooleanClass
    case MashWrapped(_: Instant)                           ⇒ DateTimeClass
    case MashWrapped(_: LocalDate)                         ⇒ DateClass
    case userClass: UserDefinedClass                       ⇒ getUserClassType(userClass)
    case _: MashClass                                      ⇒ ClassClass
    case MashUnit                                          ⇒ UnitClass
    case xs: MashList                                      ⇒ xs.elements.headOption.map(getType).getOrElse(Type.Any).seq
    case obj@MashObject(_, None)                           ⇒ getSimpleObjectType(obj)
    case obj@MashObject(_, Some(GroupClass))               ⇒ getTypeOfGroup(obj)
    case obj@MashObject(_, Some(TimedResultClass))         ⇒ getTypeOfTimedResult(obj)
    case MashObject(_, Some(userClass: UserDefinedClass))  ⇒ Type.UserClassInstance(getUserClassType(userClass))
    case MashObject(_, Some(klass))                        ⇒ klass
    case _                                                 ⇒ Type.Any
  }

  private def getSimpleObjectType(obj: MashObject) = {
    val fields = for { (field, value) ← obj.stringFields } yield field -> getType(value)
    Type.Object(fields)
  }

  private def getUserFunctionType(function: AnonymousFunction): Type.UserDefinedFunction = {
    val AnonymousFunction(parameterModel, body, context) = function
    val functionBindings = buildBindings(context.scopeStack.bindings)
    Type.UserDefinedFunction(docCommentOpt = None, isPrivate = false, None, parameterModel, body, functionBindings)
  }

  private def getUserFunctionType(function: UserDefinedFunction): Type.UserDefinedFunction = {
    val UserDefinedFunction(docCommentOpt, name, parameterModel, body, context, _) = function
    val functionBindings = buildBindings(context.scopeStack.bindings)
    Type.UserDefinedFunction(docCommentOpt, isPrivate = false, Some(name), parameterModel, body, functionBindings)
  }

  private def getUserClassType(userClass: UserDefinedClass): Type.UserClass = {
    val UserDefinedClass(_, name, _, params, methods) = userClass
    Type.UserClass(name, params, getMethodTypes(methods))
  }

  private def getBoundMethodType(target: MashValue, method: UserDefinedMethod) = {
    val UserDefinedMethod(docCommentOpt, name, params, _, body, context, isPrivate, _) = method
    val bindings = buildBindings(context.scopeStack.bindings)
    val methodType = Type.UserDefinedFunction(docCommentOpt, isPrivate, Some(name), params, body, bindings)
    Type.BoundUserDefinedMethod(getType(target), methodType)
  }

  def instanceType(userClass: UserDefinedClass): Type.UserClassInstance = {
    val UserDefinedClass(_, name, _, params, methods) = userClass
    Type.UserClassInstance(Type.UserClass(name, params, getMethodTypes(methods)))
  }

  private def getMethodTypes(methods: Seq[UserDefinedMethod]): ListMap[String, Type.UserDefinedFunction] = {
    var methodBindings = Map[String, Type]() // TODO: Should also include parent methods
    val methodNameTypePairs: ArrayBuffer[(String, Type.UserDefinedFunction)] = ArrayBuffer()
    for (method ← methods) {
      val bindings = methodBindings ++ buildBindings(method.context.scopeStack.bindings)
      val functionType = Type.UserDefinedFunction(method.docCommentOpt, method.isPrivate, Some(method.name),
        method.params, method.body, bindings)
      for (name ← method.name +: method.aliases) {
        methodNameTypePairs += name -> functionType
        methodBindings += name -> functionType
      }
    }
    ListMap(methodNameTypePairs: _*)
  }

  private def getTypeOfTimedResult(obj: MashObject): Type = {
    val resultType = obj.get(TimedResultClass.Fields.Result).map(getType) getOrElse Type.Any
    TimedResultClass.withGenerics(resultType)
  }

  private def getTypeOfGroup(obj: MashObject): Type = {
    val groupTypeOpt =
      for {
        key ← obj.get(GroupClass.Fields.Key)
        keyType = getType(key)
        values ← obj.get(GroupClass.Fields.Values)
        valueType = getType(values) match {
          case Type.Seq(elementType) ⇒ elementType
          case _                     ⇒ Type.Any
        }
      } yield GroupClass.withGenerics(keyType, valueType)
    groupTypeOpt.getOrElse(GroupClass.withGenerics(Type.Any, Type.Any))
  }

}