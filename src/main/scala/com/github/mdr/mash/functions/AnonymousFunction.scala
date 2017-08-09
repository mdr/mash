package com.github.mdr.mash.functions

import com.github.mdr.mash.evaluator.{ EvaluationContext, Evaluator }
import com.github.mdr.mash.parser.AbstractSyntax._
import com.github.mdr.mash.runtime.MashValue

import scala.PartialFunction._

case class AnonymousFunction(params: ParameterModel, body: Expr, context: EvaluationContext)
  extends MashFunction(nameOpt = None) {

  def call(boundParams: BoundParams): MashValue = {
    val newScopeStack = context.scopeStack.withLeakyScope(boundParams.boundNames.toSeq, boundParams.safeNames)
    Evaluator.evaluate(body)(context.copy(scopeStack = newScopeStack))
  }

  override def paramContext = context

  override def summaryOpt = Some("anonymous function")

  override def equals(that: Any): Boolean = cond(that) { case anyRef: AnyRef ⇒ anyRef eq this }

  override def hashCode = System.identityHashCode(this)

}