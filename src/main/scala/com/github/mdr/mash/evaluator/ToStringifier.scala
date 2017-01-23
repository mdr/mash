package com.github.mdr.mash.evaluator

import java.util.IdentityHashMap

import com.github.mdr.mash.functions.MashMethod
import com.github.mdr.mash.ns.core.AnyClass
import com.github.mdr.mash.runtime.{ MashString, MashValue }

import scala.util.Try

abstract class AbstractToStringMethod extends MashMethod(AnyClass.ToStringMethod.name) {

  val params = AnyClass.ToStringMethod.params

  def apply(target: MashValue, arguments: Arguments): MashString = {
    params.validate(arguments)
    MashString(toString(target))
  }

  protected def toString(mashValue: MashValue): String

  override def typeInferenceStrategy = AnyClass.ToStringMethod.typeInferenceStrategy

  override def summary = AnyClass.ToStringMethod.summary

}

object ToStringifier {

  val visited = new ThreadLocal[IdentityHashMap[MashValue, Unit]] {
    override def initialValue = new IdentityHashMap
  }

  def visit[T](x: MashValue, alreadyVisited: T)(f: ⇒ T): T = {
    val map = visited.get
    if (map containsKey x)
      alreadyVisited
    else {
      map.put(x, ())
      try
        f
      finally
        map.remove(x)
    }
  }

  def stringify(x: MashValue): String = if (x.isAString) x.asInstanceOf[MashString].s else {
    val toStringMethod = MemberEvaluator.lookup(x, AnyClass.ToStringMethod.name)
    val stringified = Evaluator.invokeNullaryFunctions(toStringMethod, locationOpt = None)
    stringified match {
      case MashString(s, _) ⇒ s
      case _                ⇒ throw new EvaluatorException("Invalid toString value of type " + stringified.typeName)
    }
  }

  def safeStringify(x: MashValue): String = Try(stringify(x)).getOrElse("error calling .toString")

}