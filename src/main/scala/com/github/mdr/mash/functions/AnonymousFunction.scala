package com.github.mdr.mash.functions

import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.evaluator.EvaluationContext
import com.github.mdr.mash.evaluator.Evaluator
import com.github.mdr.mash.runtime.MashValue
import com.github.mdr.mash.parser.AbstractSyntax._

case class AnonymousFunction(parameter: FunctionParam, body: Expr, context: EvaluationContext) extends MashFunction(nameOpt = None) {

  def apply(arguments: Arguments): MashValue = {
    val arg = arguments.positionArgs(0).value
    val newContext = context.copy(scopeStack = context.scopeStack.withLambdaScope(parameter.name, arg))
    Evaluator.evaluate(body)(newContext)
  }

  val params = ParameterModel(Seq(Parameter(parameter.name, "Argument")))

  override def summary = "anonymous function"

}