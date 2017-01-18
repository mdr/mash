package com.github.mdr.mash.evaluator

import com.github.mdr.mash.runtime.{ MashObject, MashValue }

sealed abstract class Scope(val variables: MashObject) {

  def get(name: String): Option[MashValue] = variables.get(name) orElse thisGet(name)

  def thisGet(name: String): Option[MashValue] =
    for {
      thisValue ← thisOpt
      memberValue ← MemberEvaluator.maybeLookup(thisValue, name)
    } yield memberValue

  def set(name: String, value: MashValue) = variables.set(name, value)

  val thisOpt: Option[MashValue]

}

/**
  * A leaky scope lets writes escape out of it
  *
  * e.g. the body of lambdas, or inside a { block... }
  */
case class LeakyScope(override val variables: MashObject) extends Scope(variables) {
  override val thisOpt: Option[MashValue] = None
}

/**
  * A full scope doesn't let writes escape.
  *
  * e.g. the body of def-defined functions, or a compilation unit
  */
case class FullScope(override val variables: MashObject,
                     thisOpt: Option[MashValue] = None) extends Scope(variables)

object ScopeStack {

  def apply(globalVariables: MashObject): ScopeStack =
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
        if (innermostScope.variables hasField name)
          innermostScope.set(name, value)
        else innermostScope.thisOpt.collect { case obj: MashObject if obj hasField name ⇒ obj } match {
          case Some(thisObject) ⇒
            thisObject.set(name, value)
          case None             ⇒
            innermostScope match {
              case leakyScope: LeakyScope                                        ⇒ set(name, value, outerScopes)
              case _                                                             ⇒ innermostScope.set(name, value)
            }
        }
      case Nil                           ⇒
        throw new AssertionError("Missing scope")
    }

  def withLeakyScope(nameValues: Seq[(String, MashValue)]) = withScope(LeakyScope(MashObject.of(nameValues)))

  def withFullScope(scopeObject: MashObject) = withScope(FullScope(scopeObject))

  def withFullScope(bindings: Map[String, MashValue]) = withScope(FullScope(MashObject.of(bindings)))

  def withFullScope(bindings: Map[String, MashValue], thisValue: MashValue) =
    withScope(FullScope(MashObject.of(bindings), thisOpt = Some(thisValue)))

  private def withScope(scope: Scope) = ScopeStack(scope :: scopes)

  def bindings: Map[String, MashValue] = bindings(scopes)

  private def bindings(scopes: List[Scope]): Map[String, MashValue] = scopes match {
    case Nil           ⇒ Map()
    case scope :: rest ⇒ bindings(rest) ++ scope.variables.immutableFields
  }

}