package com.github.mdr.mash.evaluator

import com.github.mdr.mash.ns.os.PathClass
import com.github.mdr.mash.ns.os.ProcessResultClass
import com.github.mdr.mash.parser.AbstractSyntax._
import com.github.mdr.mash.runtime._
import com.github.mdr.mash.subprocesses.ProcessRunner

object MishEvaluator {

  def evaluateMishInterpolation(expr: MishInterpolation)(implicit context: EvaluationContext) =
    expr.part match {
      case StringPart(s)  ⇒ MashString(s, PathClass)
      case ExprPart(expr) ⇒ Evaluator.evaluate(expr)
    }

  def evaluateMishExpr(expr: MishExpr)(implicit context: EvaluationContext): MashValue = {
    val MishExpr(command, args, captureProcessOutput, _) = expr
    val evaluatedCommand = Evaluator.evaluate(command)
    val evaluatedArgs = args.map(Evaluator.evaluate(_))
    val flattenedArgs: Seq[MashValue] = evaluatedArgs.flatMap {
      case xs: MashList ⇒ xs.items
      case x            ⇒ Seq(x)
    }
    val allArgs = evaluatedCommand +: flattenedArgs
    if (captureProcessOutput) {
      val processResult = ProcessRunner.runProcess(allArgs, expandTilde = true, captureProcess = captureProcessOutput)
      ProcessResultClass.fromResult(processResult)
    } else {
      ProcessRunner.runProcess(allArgs, expandTilde = true)
      MashUnit
    }
  }

}