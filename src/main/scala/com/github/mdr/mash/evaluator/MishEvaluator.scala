package com.github.mdr.mash.evaluator

import com.github.mdr.mash.ns.os.PathClass
import com.github.mdr.mash.ns.os.ProcessResultClass
import com.github.mdr.mash.parser.AbstractSyntax._
import com.github.mdr.mash.runtime._
import com.github.mdr.mash.subprocesses.ProcessRunner
import java.nio.file.Paths
import com.github.mdr.mash.os.linux.LinuxEnvironmentInteractions
import com.github.mdr.mash.ns.os.ChangeDirectoryFunction
import com.github.mdr.mash.parser.RedirectOperator
import java.nio.file.Path

object MishEvaluator extends EvaluatorHelper {

  private val environmentInteractions = LinuxEnvironmentInteractions

  def evaluateMishInterpolation(expr: MishInterpolation)(implicit context: EvaluationContext) =
    expr.part match {
      case StringPart(s)  ⇒ MashString(s, PathClass)
      case ExprPart(expr) ⇒ Evaluator.evaluate(expr)
    }

  def evaluateMishExpr(expr: MishExpr)(implicit context: EvaluationContext): MashValue = {
    val MishExpr(command, args, redirects, captureProcessOutput, _) = expr
    val evaluatedCommand = Evaluator.evaluate(command)
    val evaluatedArgs = args.map(Evaluator.evaluate)
    def getRedirect(operator: RedirectOperator): Option[Path] =
      redirects.find(_.operator == operator).map(redirect ⇒ Paths.get(ToStringifier.stringify(Evaluator.evaluate(redirect.arg))))
    val stdinRedirectOpt = getRedirect(RedirectOperator.StandardInput)
    val stdoutRedirectOpt = getRedirect(RedirectOperator.StandardOutput)
    evaluatedCommand match {
      case MashString("cd", _) ⇒
        evaluateCd(evaluatedArgs, args)
      case _ ⇒
        val flattenedArgs: Seq[MashValue] = evaluatedArgs.flatMap {
          case xs: MashList ⇒ xs.items
          case x            ⇒ Seq(x)
        }
        val allArgs = evaluatedCommand +: flattenedArgs
        if (captureProcessOutput) {
          val processResult = ProcessRunner.runProcess(allArgs, captureProcess = captureProcessOutput, stdinRedirectOpt = stdinRedirectOpt, stdoutRedirectOpt = stdoutRedirectOpt)
          ProcessResultClass.fromResult(processResult)
        } else {
          ProcessRunner.runProcess(allArgs, stdinRedirectOpt = stdinRedirectOpt, stdoutRedirectOpt = stdoutRedirectOpt)
          MashUnit
        }
    }
  }

  private def evaluateCd(args: Seq[MashValue], argExprs: Seq[Expr])(implicit context: EvaluationContext): MashUnit = {
    import ChangeDirectoryFunction._
    args match {
      case Seq() ⇒
        val home = LinuxEnvironmentInteractions.home
        changeDirectory(home) match {
          case Success       ⇒ MashUnit
          case NotADirectory ⇒ throw new EvaluatorException(s"Could not change directory to '$home', not a directory")
        }
      case Seq(pathValue) ⇒
        val path = Paths.get(ToStringifier.stringify(pathValue))
        changeDirectory(path) match {
          case Success       ⇒ MashUnit
          case NotADirectory ⇒ throw new EvaluatorException(s"Could not change directory to '$path', not a directory", sourceLocation(argExprs(0)))
        }
      case _ ⇒
        throw new EvaluatorException("Too many arguments to 'cd'", sourceLocation(argExprs(1)))
    }
  }

}