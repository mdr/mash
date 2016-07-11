package com.github.mdr.mash.functions

import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.evaluator.EvaluationContext
import com.github.mdr.mash.evaluator.Evaluator
import com.github.mdr.mash.runtime.MashValue
import com.github.mdr.mash.parser.AbstractSyntax._

case class AnonymousFunction(parameterList: FunctionParamList, body: Expr, context: EvaluationContext) extends MashFunction(nameOpt = None) {

  def apply(arguments: Arguments): MashValue = {
    val nameValues = parameterList.params.map(_.name).zip(arguments.positionArgs.map(_.value))
    val newContext = context.copy(scopeStack = context.scopeStack.withLambdaScope(nameValues))
    Evaluator.evaluate(body)(newContext)
  }

  val params = {
    val parameters = parameterList.params.map(p ⇒ Parameter(p.name, "Argument"))
    ParameterModel(parameters)
  }

  override def summary = "anonymous function"

}