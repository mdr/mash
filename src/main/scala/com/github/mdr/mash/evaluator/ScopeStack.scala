package com.github.mdr.mash.evaluator

import com.github.mdr.mash.runtime.{ MashObject, MashValue }

case class Binding(value: MashValue, isSafe: Boolean = false)

sealed abstract class Scope {

  val variables: MashObject

  val safeNames: Set[String]

  val thisOpt: Option[MashValue]

  def get(name: String): Option[Binding] = variables.get(name).map(Binding(_, isSafe = safeNames contains name)) orElse thisGet(name)

  private def thisGet(name: String): Option[Binding] =
    for {
      thisValue ← thisOpt
      memberValue ← MemberEvaluator.maybeLookupByString(thisValue, name, includePrivate = true, includeShyMembers = false)
    } yield Binding(memberValue)

  def set(name: String, value: MashValue) = variables.set(name, value)
}

/**
  * A leaky scope lets writes escape out of it
  *
  * e.g. the body of lambdas, or inside a { block... }
  */
case class LeakyScope(variables: MashObject,
                      safeNames: Set[String] = Set()) extends Scope {
  override val thisOpt: Option[MashValue] = None
}

/**
  * A full scope doesn't let writes escape.
  *
  * e.g. the body of def-defined functions, or a compilation unit
  */
case class FullScope(variables: MashObject,
                     safeNames: Set[String] = Set(),
                     thisOpt: Option[MashValue] = None) extends Scope

object ScopeStack {

  def apply(globalVariables: MashObject): ScopeStack =
    ScopeStack(List(FullScope(globalVariables)))

}

case class ScopeStack(scopes: List[Scope]) {

  def lookup(name: String): Option[Binding] = lookup(name, scopes)

  def thisOpt: Option[MashValue] = thisOpt(scopes)

  private def thisOpt(scopes: List[Scope]): Option[MashValue] =
    scopes match {
      case Nil           ⇒ None
      case scope :: rest ⇒ scope.thisOpt orElse thisOpt(rest)
    }

  private def lookup(name: String, scopes: List[Scope]): Option[Binding] =
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
              case leakyScope: LeakyScope ⇒ set(name, value, outerScopes)
              case _                      ⇒ innermostScope.set(name, value)
            }
        }
      case Nil                           ⇒
        throw new AssertionError("Missing scope")
    }

  def withLeakyScope(nameValues: Seq[(String, MashValue)] = Seq(),
                     safeNames: Set[String] = Set()): ScopeStack =
    withScope(LeakyScope(MashObject.of(nameValues), safeNames))

  def withFullScope(scopeObject: MashObject) = withScope(FullScope(scopeObject))

  def withFullScope(bindings: Map[String, MashValue],
                    safeNames: Set[String]): ScopeStack =
    withScope(FullScope(MashObject.of(bindings), safeNames))

  def withFullScope(bindings: Map[String, MashValue], safeNames: Set[String], thisValue: MashValue) =
    withScope(FullScope(MashObject.of(bindings), safeNames, thisOpt = Some(thisValue)))

  private def withScope(scope: Scope) = ScopeStack(scope :: scopes)

  def bindings: Map[String, MashValue] = bindings(scopes)

  private def bindings(scopes: List[Scope]): Map[String, MashValue] = scopes match {
    case Nil           ⇒ Map()
    case scope :: rest ⇒ bindings(rest) ++ scope.variables.stringFields
  }

}