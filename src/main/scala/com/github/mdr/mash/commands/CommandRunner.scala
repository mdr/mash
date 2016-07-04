package com.github.mdr.mash.commands

import java.io.PrintStream

import org.fusesource.jansi.Ansi

import com.github.mdr.mash.DebugLogger
import com.github.mdr.mash.Singletons
import com.github.mdr.mash.compiler.CompilationSettings
import com.github.mdr.mash.compiler.CompilationUnit
import com.github.mdr.mash.compiler.Compiler
import com.github.mdr.mash.evaluator._
import com.github.mdr.mash.evaluator.StandardEnvironment
import com.github.mdr.mash.parser.AbstractSyntax
import com.github.mdr.mash.parser.MashParserException
import com.github.mdr.mash.parser.ParseError
import com.github.mdr.mash.printer.PrintResult
import com.github.mdr.mash.printer.Printer
import com.github.mdr.mash.runtime.MashObject
import com.github.mdr.mash.runtime.MashUnit
import com.github.mdr.mash.runtime.MashValue
import com.github.mdr.mash.terminal.TerminalInfo

class CommandRunner(output: PrintStream, terminalInfo: TerminalInfo, globals: MashObject) {

  val errorPrinter = new ErrorPrinter(output, terminalInfo)
  val debugCommandRunner = new DebugCommandRunner(output, globals)

  def run(cmd: String, unitName: String, mish: Boolean = false, bareWords: Boolean): CommandResult = {
    cmd match {
      case DebugCommand(keyword, args) ⇒
        debugCommandRunner.runDebugCommand(keyword, args, bareWords)
        CommandResult()
      case MishCommand(prefix, mishCmd) ⇒
        if (mishCmd == "")
          CommandResult(toggleMish = true)
        else {
          val unit = CompilationUnit(mishCmd, unitName, interactive = true, mish = true)
          runCommandAndPrint(unit, bareWords = bareWords)
        }
      case SuffixMishCommand(mishCmd, suffix) ⇒
        val unit = CompilationUnit(mishCmd, unitName, interactive = true, mish = true)
        runCommandAndPrint(unit, bareWords = bareWords)
      case _ ⇒
        val unit = CompilationUnit(cmd, unitName, interactive = true, mish = mish)
        runCommandAndPrint(unit, bareWords = bareWords)
    }
  }

  def runCompilationUnit(unit: CompilationUnit, bareWords: Boolean): Option[MashValue] =
    safeCompile(unit, bareWords = bareWords).map { expr ⇒
      runExpr(expr, unit)
    }

  private def runCommandAndPrint(unit: CompilationUnit, bareWords: Boolean): CommandResult =
    runCompilationUnit(unit, bareWords).map { result ⇒
      val printer = new Printer(output, terminalInfo)
      val PrintResult(objectTableModelOpt) = printer.print(result)
      CommandResult(Some(result), objectTableModelOpt = objectTableModelOpt)
    }.getOrElse(CommandResult())

  private def runExpr(expr: AbstractSyntax.Expr, unit: CompilationUnit): MashValue =
    try {
      val context = new ExecutionContext(Thread.currentThread)
      Singletons.environment = globals.get(StandardEnvironment.Env) match {
        case Some(obj: MashObject) ⇒ obj
        case _                     ⇒ MashObject.empty()
      }
      Singletons.setExecutionContext(context)
      ExecutionContext.set(context)
      Evaluator.evaluate(expr)(EvaluationContext(ScopeStack(globals.fields)))
    } catch {
      case e @ EvaluatorException(msg, stack, cause) ⇒
        errorPrinter.printError("Error", msg, unit, stack.reverse)
        DebugLogger.logException(e)
        MashUnit
      case _: EvaluationInterruptedException ⇒
        output.println(Ansi.ansi().fg(Ansi.Color.YELLOW).bold.a("Interrupted:").boldOff.a(" command cancelled by user").reset())
        MashUnit
    }

  private def safeCompile(unit: CompilationUnit, bareWords: Boolean): Option[AbstractSyntax.Expr] = {
    val settings = CompilationSettings(inferTypes = false, bareWords = bareWords)
    Compiler.compile(unit, globals.immutableFields, settings) match {
      case Left(ParseError(msg, location)) ⇒
        errorPrinter.printError("Syntax error", msg, unit, Seq(SourceLocation(unit.provenance, location)))
        None
      case Right(expr) ⇒
        Some(expr)
    }
  }

}