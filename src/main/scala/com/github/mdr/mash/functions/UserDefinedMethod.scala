package com.github.mdr.mash.functions

import com.github.mdr.mash.evaluator.{ Arguments, EvaluationContext, Evaluator }
import com.github.mdr.mash.parser.AbstractSyntax.{ Expr, ParamList }
import com.github.mdr.mash.runtime.MashValue

case class UserDefinedMethod(
    methodName: String,
    params: ParameterModel,
    paramList: ParamList,
    body: Expr,
    context: EvaluationContext) extends MashMethod(methodName) {

  override def apply(target: MashValue, arguments: Arguments): MashValue = {
    val parameterEvalContext = Some(context.copy(scopeStack = context.scopeStack.withFullScope(Map(), target)))
    val boundParams = Evaluator.parameterModel(paramList, parameterEvalContext).validate(arguments)
    val methodBodyEvalContext = context.copy(scopeStack = context.scopeStack.withFullScope(boundParams.boundNames, target))
    Evaluator.evaluate(body)(methodBodyEvalContext)
  }

  override def summary: String = s"Method '$methodName'"

}