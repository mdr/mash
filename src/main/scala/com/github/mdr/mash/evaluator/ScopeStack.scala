package com.github.mdr.mash.evaluator

import scala.collection.mutable
import com.github.mdr.mash.runtime.MashValue

case class Scope(variables: mutable.Map[String, MashValue], canAssignHere: Boolean) {
  def get(name: String): Option[MashValue] = variables.get(name)
  def set(name: String, value: MashValue) = variables += name -> value
}

object ScopeStack {
  
  def apply(globalVariables: mutable.Map[String, MashValue]): ScopeStack = 
    ScopeStack(List(Scope(globalVariables, canAssignHere = true)))
  
}

case class ScopeStack(scopes: List[Scope]) {

  def lookup(name: String): Option[MashValue] = lookup(name, scopes)

  private def lookup(name: String, scopes: List[Scope]): Option[MashValue] = scopes match {
    case Nil           ⇒ None
    case scope :: rest ⇒ scope.get(name).orElse(lookup(name, rest))
  }

  def set(name: String, value: MashValue) {
    scopes.find(_.canAssignHere).get.set(name, value)
  }

  def withLambdaScope(name: String, value: MashValue) = {
    val scope = Scope(mutable.Map[String, MashValue](name -> value), canAssignHere = false)
    ScopeStack(scope :: scopes)
  }

  def withFunctionCallScope(bindings: Map[String, MashValue]) = {
    val scope = Scope(mutable.Map[String, MashValue](bindings.toSeq: _*), canAssignHere = true)
    ScopeStack(scope :: scopes)
  }

  def bindings: Map[String, MashValue] = bindings(scopes)

  private def bindings(scopes: List[Scope]): Map[String, MashValue] = scopes match {
    case Nil           ⇒ Map()
    case scope :: rest ⇒ bindings(rest) ++ scope.variables
  }

}