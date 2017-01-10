package com.github.mdr.mash.functions

import com.github.mdr.mash.evaluator.{ Arguments, EvaluationContext, Evaluator }
import com.github.mdr.mash.parser.AbstractSyntax.Expr
import com.github.mdr.mash.runtime.MashValue

case class UserDefinedMethod(
    methodName: String,
    params: ParameterModel,
    body: Expr,
    context: EvaluationContext) extends MashMethod(methodName) {

  override def apply(target: MashValue, arguments: Arguments): MashValue = {
    val boundParams = params.validate(arguments)
    val newScopeStack = context.scopeStack.withFullScope(boundParams.boundNames, target)
    Evaluator.evaluate(body)(context.copy(scopeStack = newScopeStack))
  }

  override def summary: String = s"Method '$methodName'"

}