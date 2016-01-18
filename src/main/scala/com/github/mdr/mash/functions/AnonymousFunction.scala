package com.github.mdr.mash.functions

import com.github.mdr.mash.parser.AbstractSyntax.Expr
import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.evaluator.Environment
import com.github.mdr.mash.evaluator.Evaluator

case class AnonymousFunction(parameter: String, body: Expr, env: Environment) extends MashFunction(nameOpt = None) {

  def apply(arguments: Arguments): Any = {
    val arg = arguments.positionArgs(0)
    val newEnv = env.addBinding(parameter, arg)
    Evaluator.evaluate(body, newEnv)
  }

  val params = ParameterModel(Seq(Parameter("arg", "Argument")))

  override def summary = "anonymous function"

}