package com.github.mdr.mash.functions

import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.evaluator.EvaluationContext
import com.github.mdr.mash.evaluator.Evaluator
import com.github.mdr.mash.parser.AbstractSyntax.Expr
import com.github.mdr.mash.runtime.MashValue

case class AnonymousFunction(parameter: String, body: Expr, context: EvaluationContext) extends MashFunction(nameOpt = None) {

  def apply(arguments: Arguments): MashValue = {
    val arg = arguments.positionArgs(0).value
    val newCtx = context.copy(scopeStack = context.scopeStack.withLambdaScope(parameter, arg))
    Evaluator.evaluate(body)(newCtx)
  }

  val params = ParameterModel(Seq(Parameter(parameter, "Argument")))

  override def summary = "anonymous function"

}