package com.github.mdr.mash.evaluator

import com.github.mdr.mash.compiler.{ CompilationSettings, CompilationUnit, Compiler }
import com.github.mdr.mash.parser.ParseError
import com.github.mdr.mash.runtime.MashValue

object TestEvaluator {

  def evaluate(s: String): MashValue = {
    val env = StandardEnvironment.create
    val bindings = env.bindings
    val settings = CompilationSettings()
    val expr = Compiler.compile(CompilationUnit(s), bindings = bindings, settings) match {
      case Left(ParseError(message, _)) ⇒ throw new AssertionError("Compilation failed: " + message)
      case Right(program)               ⇒ program.body
    }
    val ctx = EvaluationContext(ScopeStack(env.globalVariables))
    Evaluator.evaluate(expr)(ctx)
  }

}
