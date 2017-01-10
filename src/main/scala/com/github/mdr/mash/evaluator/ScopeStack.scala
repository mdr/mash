package com.github.mdr.mash.evaluator

import com.github.mdr.mash.runtime.{ MashObject, MashValue }

import scala.collection.mutable

sealed abstract class Scope(val variables: mutable.Map[String, MashValue]) {

  def get(name: String): Option[MashValue] = variables.get(name) orElse thisGet(name)

  def thisGet(name: String): Option[MashValue] =
    for {
      thisValue ← thisOpt
      memberValue ← MemberEvaluator.maybeLookup(thisValue, name)
    } yield memberValue

  def set(name: String, value: MashValue) = variables += name -> value

  val thisOpt: Option[MashValue]

}

/**
  * A block scope lets writes escape out of it, as long as they are already bound.
  *
  * e.g. the body of lambdas, or inside a { block }
  */
case class BlockScope(override val variables: mutable.Map[String, MashValue]) extends Scope(variables) {
  override val thisOpt: Option[MashValue] = None
}

/**
  * A full scope doesn't let writes escape.
  *
  * e.g. the body of def-defined functions
  */
case class FullScope(override val variables: mutable.Map[String, MashValue],
                     thisOpt: Option[MashValue] = None) extends Scope(variables)

object ScopeStack {

  def apply(globalVariables: mutable.Map[String, MashValue]): ScopeStack =
    ScopeStack(List(FullScope(globalVariables)))

}

case class ScopeStack(scopes: List[Scope]) {

  def lookup(name: String): Option[MashValue] = lookup(name, scopes)

  def thisOpt: Option[MashValue] = thisOpt(scopes)

  private def thisOpt(scopes: List[Scope]): Option[MashValue] =
    scopes match {
      case Nil           ⇒ None
      case scope :: rest ⇒ scope.thisOpt orElse thisOpt(rest)
    }

  private def lookup(name: String, scopes: List[Scope]): Option[MashValue] =
    scopes match {
      case Nil           ⇒ None
      case scope :: rest ⇒ scope.get(name) orElse lookup(name, rest)
    }

  def set(name: String, value: MashValue) {
    set(name, value, scopes)
  }

  private def set(name: String, value: MashValue, scopes: List[Scope]): Unit =
    scopes match {
      case innermostScope :: outerScopes ⇒
        if (innermostScope.variables.get(name).isDefined)
          innermostScope.set(name, value)
        else innermostScope.thisOpt.collect { case obj: MashObject if obj.get(name).isDefined ⇒ obj } match {
          case Some(obj) ⇒
            obj.set(name, value)
          case None      ⇒
            innermostScope match {
              case blockScope: BlockScope if lookup(name, outerScopes).isDefined ⇒ set(name, value, outerScopes)
              case _                                                      ⇒ innermostScope.set(name, value)
            }
        }
      case Nil                ⇒
        throw new AssertionError("Missing global scope")
    }

  def withBlockScope(nameValues: Seq[(String, MashValue)]) = {
    val scope = BlockScope(makeVariables(nameValues: _*))
    ScopeStack(scope :: scopes)
  }

  def withFullScope(bindings: Map[String, MashValue]) = {
    val scope = FullScope(makeVariables(bindings.toSeq: _*))
    ScopeStack(scope :: scopes)
  }

  def withFullScope(bindings: Map[String, MashValue], thisValue: MashValue) = {
    val scope = FullScope(makeVariables(bindings.toSeq: _*), thisOpt = Some(thisValue))
    ScopeStack(scope :: scopes)
  }

  private def makeVariables(pairs: (String, MashValue)*) = mutable.Map[String, MashValue](pairs: _*)

  def bindings: Map[String, MashValue] = bindings(scopes)

  private def bindings(scopes: List[Scope]): Map[String, MashValue] = scopes match {
    case Nil           ⇒ Map()
    case scope :: rest ⇒ bindings(rest) ++ scope.variables
  }

}