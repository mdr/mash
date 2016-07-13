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
import com.github.mdr.mash.parser.ParseError
import com.github.mdr.mash.printer.PrintResult
import com.github.mdr.mash.printer.Printer
import com.github.mdr.mash.runtime._
import com.github.mdr.mash.terminal.TerminalInfo
import java.util.UUID

class CommandRunner(output: PrintStream, terminalInfo: TerminalInfo, globals: MashObject, sessionId: UUID) {

  private val errorPrinter = new ErrorPrinter(output, terminalInfo)
  private val debugCommandRunner = new DebugCommandRunner(output, globals)
  private val debugLogger = new DebugLogger(sessionId)

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
          runCommandAndPrintResult(unit, bareWords = bareWords)
        }
      case SuffixMishCommand(mishCmd, suffix) ⇒
        val unit = CompilationUnit(mishCmd, unitName, interactive = true, mish = true)
        runCommandAndPrintResult(unit, bareWords = bareWords)
      case _ ⇒
        val unit = CompilationUnit(cmd, unitName, interactive = true, mish = mish)
        runCommandAndPrintResult(unit, bareWords = bareWords)
    }
  }

  def runCompilationUnit(unit: CompilationUnit, bareWords: Boolean): Option[MashValue] =
    safeCompile(unit, bareWords = bareWords).map { expr ⇒ runExpr(expr, unit) }

  private def runCommandAndPrintResult(unit: CompilationUnit, bareWords: Boolean): CommandResult =
    runCompilationUnit(unit, bareWords).map(printResult).getOrElse(CommandResult())

  private def printResult(result: MashValue): CommandResult = {
    val printer = new Printer(output, terminalInfo)
    val PrintResult(objectTableModelOpt) = printer.print(result)
    CommandResult(Some(result), objectTableModelOpt = objectTableModelOpt)
  }

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
        debugLogger.logException(e)
        MashUnit
      case _: EvaluationInterruptedException ⇒
        output.println(Ansi.ansi().fg(Ansi.Color.YELLOW).bold.a("Interrupted:").boldOff.a(" command cancelled by user").reset())
        MashUnit
    }

  private def safeCompile(unit: CompilationUnit, bareWords: Boolean): Option[AbstractSyntax.Expr] = {
    val settings = CompilationSettings(inferTypes = false, bareWords = bareWords)
    Compiler.compile(unit, globals.immutableFields, settings) match {
      case Left(ParseError(msg, location)) ⇒
        errorPrinter.printError("Syntax error", msg, unit, Seq(StackTraceItem(SourceLocation(unit.provenance, location))))
        None
      case Right(expr) ⇒
        Some(expr)
    }
  }

}