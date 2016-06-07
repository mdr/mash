package com.github.mdr.mash.functions

import com.github.mdr.mash.parser.AbstractSyntax.Expr
import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.evaluator.Environment
import com.github.mdr.mash.evaluator.Evaluator
import com.github.mdr.mash.runtime.MashValue
import com.github.mdr.mash.evaluator.EvaluationContext

case class UserDefinedFunction(
    functionName: String,
    params: ParameterModel,
    body: Expr,
    context: EvaluationContext) extends MashFunction(nameOpt = Some(functionName)) {

  def apply(arguments: Arguments): MashValue = {
    val boundParams = params.validate(arguments)
    var newEnv = context.environment
    for (param ← params.params)
      newEnv = newEnv.addBinding(param.name, boundParams(param))
    Evaluator.evaluate(body)(context.copy(environment = newEnv))
  }

  override def summary = s"User defined function '$name'"

}