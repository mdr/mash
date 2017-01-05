package com.github.mdr.mash.functions

import com.github.mdr.mash.evaluator.{ Arguments, EvaluationContext, Evaluator }
import com.github.mdr.mash.parser.AbstractSyntax.Expr
import com.github.mdr.mash.runtime.MashValue

case class UserDefinedFunction(
    functionName: String,
    params: ParameterModel,
    body: Expr,
    context: EvaluationContext) extends MashFunction(nameOpt = Some(functionName)) {

  def apply(arguments: Arguments): MashValue = {
    val boundParams = params.validate(arguments)
    val newScopeStack = context.scopeStack.withFullScope(boundParams.boundNames)
    Evaluator.evaluate(body)(context.copy(scopeStack = newScopeStack))
  }

  override def summary = s"User defined function '$name'"

}