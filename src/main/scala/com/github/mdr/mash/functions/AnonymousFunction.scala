package com.github.mdr.mash.functions

import com.github.mdr.mash.evaluator.{ EvaluationContext, Evaluator }
import com.github.mdr.mash.parser.AbstractSyntax._
import com.github.mdr.mash.runtime.MashValue

case class AnonymousFunction(params: ParameterModel, body: Expr, context: EvaluationContext)
  extends MashFunction(nameOpt = None) {

  def call(boundParams: BoundParams): MashValue = {
    val newScopeStack = context.scopeStack.withLeakyScope(boundParams.boundNames.toSeq)
    Evaluator.evaluate(body)(context.copy(scopeStack = newScopeStack))
  }

  override def paramContext = context

  override def summaryOpt = Some("anonymous function")

}