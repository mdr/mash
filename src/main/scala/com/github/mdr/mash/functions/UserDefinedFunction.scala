package com.github.mdr.mash.functions

import com.github.mdr.mash.parser.AbstractSyntax.Expr
import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.evaluator.Environment
import com.github.mdr.mash.evaluator.Evaluator

case class UserDefinedFunction(functionName: String, parameters: Seq[String], body: Expr, env: Environment) extends MashFunction(nameOpt = Some(functionName)) {

  val params = ParameterModel(parameters.map(p ⇒ Parameter(p, s"Parameter '$p'")))

  def apply(arguments: Arguments): Any = {
    val boundParams = params.validate(arguments)
    var newEnv = env
    for (param ← parameters) {
      newEnv = env.addBinding(param, boundParams(param))
    }
    Evaluator.evaluate(body, newEnv)
  }

  override def summary = s"User defined function '$name'"

}