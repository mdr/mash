package com.github.mdr.mash.evaluator

import com.github.mdr.mash.runtime.MashValue

import scala.collection.mutable

sealed abstract class Scope(val variables: mutable.Map[String, MashValue]) {
  def get(name: String): Option[MashValue] = variables.get(name)
  def set(name: String, value: MashValue) = variables += name -> value
}
case class LambdaScope(override val variables: mutable.Map[String, MashValue]) extends Scope(variables)
case class FullScope(override val variables: mutable.Map[String, MashValue]) extends Scope(variables)

object ScopeStack {

  def apply(globalVariables: mutable.Map[String, MashValue]): ScopeStack =
    ScopeStack(List(FullScope(globalVariables)))

}

case class ScopeStack(scopes: List[Scope]) {

  def lookup(name: String): Option[MashValue] = lookup(name, scopes)

  private def lookup(name: String, scopes: List[Scope]): Option[MashValue] =
    scopes match {
      case Nil           ⇒ None
      case scope :: rest ⇒ scope.get(name).orElse(lookup(name, rest))
    }

  def set(name: String, value: MashValue) {
    set(name, value, scopes)
  }

  private def set(name: String, value: MashValue, scopes: List[Scope]): Unit =
    scopes match {
      case (scope: LambdaScope) :: rest ⇒
        if (scope.get(name).isDefined) // Really should be handled at compilation
          throw new EvaluatorException("Cannot assign to a parameter")
        else
          set(name, value, rest)
      case (scope: FullScope) :: rest ⇒
        scope.set(name, value)
      case Nil ⇒
        throw new AssertionError("Missing global scope")
    }

  def withEmptyScope = {
    val scope = FullScope(makeVariables())
    ScopeStack(scope :: scopes)
  }

  def withLambdaScope(nameValues: Seq[(String, MashValue)]) = {
    val scope = LambdaScope(makeVariables(nameValues: _*))
    ScopeStack(scope :: scopes)
  }

  def withFunctionCallScope(bindings: Map[String, MashValue]) = {
    val scope = FullScope(makeVariables(bindings.toSeq: _*))
    ScopeStack(scope :: scopes)
  }

  private def makeVariables(pairs: (String, MashValue)*) = mutable.Map[String, MashValue](pairs: _*)

  def bindings: Map[String, MashValue] = bindings(scopes)

  private def bindings(scopes: List[Scope]): Map[String, MashValue] = scopes match {
    case Nil           ⇒ Map()
    case scope :: rest ⇒ bindings(rest) ++ scope.variables
  }

}