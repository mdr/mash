package com.github.mdr.mash.functions

import com.github.mdr.mash.parser.AbstractSyntax.Expr
import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.evaluator.Environment
import com.github.mdr.mash.evaluator.Evaluator
import com.github.mdr.mash.runtime.MashValue
import com.github.mdr.mash.evaluator.EvaluationContext

class UserDefinedFunction(
    functionName: String,
    val params: ParameterModel,
    body: Expr,
    context: EvaluationContext) extends MashFunction(nameOpt = Some(functionName)) {

  def apply(arguments: Arguments): MashValue = {
    val boundParams = params.validate(arguments)
    val pairs =
      for (param ← params.params)
        yield param.name -> boundParams(param)
    val newScopeStack = context.scopeStack.withFunctionCallScope(pairs.toMap)
    Evaluator.evaluate(body)(context.copy(scopeStack = newScopeStack))
  }

  override def summary = s"User defined function '$name'"

 
}