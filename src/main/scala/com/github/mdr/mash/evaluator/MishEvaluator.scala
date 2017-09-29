package com.github.mdr.mash.evaluator

import java.nio.file.{ Path, Paths }

import com.github.mdr.mash.Singletons
import com.github.mdr.mash.ns.os.{ PathClass, ProcessResultClass }
import com.github.mdr.mash.os.CurrentDirectoryManager
import com.github.mdr.mash.os.CurrentDirectoryManager.{ NotADirectory, Success }
import com.github.mdr.mash.os.linux.{ LinuxEnvironmentInteractions, LinuxFileSystem }
import com.github.mdr.mash.parser.AbstractSyntax._
import com.github.mdr.mash.parser.RedirectOperator
import com.github.mdr.mash.runtime._
import com.github.mdr.mash.subprocesses.ProcessRunner

object MishEvaluator extends EvaluatorHelper {

  private val environmentInteractions = LinuxEnvironmentInteractions
  private val fileSystem = LinuxFileSystem
  private val currentDirectoryManager = CurrentDirectoryManager(fileSystem, Singletons.workingDirectoryStack)

  def evaluateMishInterpolation(expr: MishInterpolation)(implicit context: EvaluationContext) =
    expr.part match {
      case StringPart(s)  ⇒ MashString(s, PathClass)
      case ExprPart(expr) ⇒ Evaluator.evaluate(expr)
    }

  def evaluateMishExpr(expr: MishExpr, stdinValueOpt: Option[MashValue] = None)(implicit context: EvaluationContext): MashValue = {
    val MishExpr(command, args, redirects, captureProcessOutput, _) = expr
    val evaluatedCommand = Evaluator.evaluate(command)
    val evaluatedArgs = args.map(Evaluator.evaluate)
    def getRedirect(operator: RedirectOperator): Option[Path] =
      redirects.find(_.operator == operator).map(redirect ⇒ Paths.get(ToStringifier.stringify(Evaluator.evaluate(redirect.arg))))
    val stdinRedirectOpt = getRedirect(RedirectOperator.StandardInput)
    val stdoutRedirectOpt = getRedirect(RedirectOperator.StandardOutput)
    val stdinImmediateOpt = stdinValueOpt map {
      case xs: MashList ⇒ xs.immutableElements.map(ToStringifier.stringify).mkString("\n")
      case x            ⇒ ToStringifier.stringify(x)
    }
    evaluatedCommand match {
      case MashString("cd", _) ⇒
        evaluateCd(evaluatedArgs, args)
      case _                   ⇒
        val flattenedArgs: Seq[MashValue] = evaluatedArgs.flatMap {
          case xs: MashList ⇒ xs.elements
          case x            ⇒ Seq(x)
        }
        val allArgs = evaluatedCommand +: flattenedArgs
        val processResult = ProcessRunner.runProcess(allArgs, captureProcess = captureProcessOutput, stdinRedirectOpt = stdinRedirectOpt, stdoutRedirectOpt = stdoutRedirectOpt, stdinImmediateOpt = stdinImmediateOpt)
        if (captureProcessOutput)
          ProcessResultClass.fromResult(processResult)
        else
          MashUnit
    }
  }

  private def evaluateCd(args: Seq[MashValue], argExprs: Seq[Expr])(implicit context: EvaluationContext): MashUnit = {
    args match {
      case Seq()          ⇒
        val home = environmentInteractions.home
        currentDirectoryManager.changeDirectory(home) match {
          case Success       ⇒ MashUnit
          case NotADirectory ⇒ throw EvaluatorException(s"Could not change directory to '$home', not a directory")
        }
      case Seq(pathValue) ⇒
        val path = Paths.get(ToStringifier.stringify(pathValue))
        currentDirectoryManager.changeDirectory(path) match {
          case Success       ⇒ MashUnit
          case NotADirectory ⇒ throw EvaluatorException(s"Could not change directory to '$path', not a directory", sourceLocation(argExprs(0)))
        }
      case _              ⇒
        throw EvaluatorException("Too many arguments to 'cd'", sourceLocation(argExprs(1)))
    }
  }

}