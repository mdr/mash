package com.github.mdr.mash.functions

import com.github.mdr.mash.parser.AbstractSyntax.Expr
import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.evaluator.Environment
import com.github.mdr.mash.evaluator.Evaluator
import com.github.mdr.mash.runtime.MashValue
import com.github.mdr.mash.evaluator.EvaluationContext

case class AnonymousFunction(parameter: String, body: Expr, evaluationContext: EvaluationContext) extends MashFunction(nameOpt = None) {

  def apply(arguments: Arguments): MashValue = {
    val arg = arguments.positionArgs(0).value
    val newCtx = evaluationContext.copy(environment = evaluationContext.environment.addBinding(parameter, arg))
    Evaluator.evaluate(body)(newCtx)
  }

  val params = ParameterModel(Seq(Parameter("arg", "Argument")))

  override def summary = "anonymous function"

}