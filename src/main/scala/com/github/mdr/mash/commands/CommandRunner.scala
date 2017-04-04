package com.github.mdr.mash.commands

import java.io.PrintStream
import java.util.UUID

import com.github.mdr.mash.compiler.{ CompilationSettings, CompilationUnit, Compiler }
import com.github.mdr.mash.evaluator.{ StandardEnvironment, _ }
import com.github.mdr.mash.parser.{ AbstractSyntax, ParseError }
import com.github.mdr.mash.printer.{ PrintResult, Printer, ViewConfig }
import com.github.mdr.mash.runtime._
import com.github.mdr.mash.screen.{ BasicColour, Screen }
import com.github.mdr.mash.terminal.TerminalInfo
import com.github.mdr.mash.{ DebugLogger, Singletons }
import com.github.mdr.mash.screen.Style.StylableString

class CommandRunner(output: PrintStream,
                    terminalInfo: TerminalInfo,
                    globals: MashObject,
                    sessionId: UUID,
                    printErrors: Boolean = true) {

  private val errorPrinter = new ErrorPrinter(output, terminalInfo)
  private val debugCommandRunner = new DebugCommandRunner(output, globals)
  private val debugLogger = new DebugLogger(sessionId.toString)

  def run(cmd: String, unitName: String, mish: Boolean, bareWords: Boolean, viewConfig: ViewConfig): CommandResult = {
    cmd match {
      case DebugCommand(keyword, args)        ⇒
        debugCommandRunner.runDebugCommand(keyword, args, bareWords)
        CommandResult()
      case MishCommand(prefix, mishCmd)       ⇒
        if (mishCmd == "")
          CommandResult(toggleMish = true)
        else {
          val unit = CompilationUnit(mishCmd, unitName, interactive = true, mish = true)
          runCommandAndPrintResult(unit, bareWords = bareWords, viewConfig = viewConfig)
        }
      case SuffixMishCommand(mishCmd, suffix) ⇒
        val unit = CompilationUnit(mishCmd, unitName, interactive = true, mish = true)
        runCommandAndPrintResult(unit, bareWords = bareWords, viewConfig = viewConfig)
      case _                                  ⇒
        val unit = CompilationUnit(cmd, unitName, interactive = true, mish = mish)
        runCommandAndPrintResult(unit, bareWords = bareWords, viewConfig = viewConfig)
    }
  }

  def runCompilationUnit(unit: CompilationUnit, bareWords: Boolean): Option[MashValue] =
    safeCompile(unit, bareWords = bareWords, printErrors = printErrors).flatMap(runProgram(_, unit))

  private def runCommandAndPrintResult(unit: CompilationUnit, bareWords: Boolean, viewConfig: ViewConfig): CommandResult =
    runCompilationUnit(unit, bareWords).map(printResult(viewConfig)).getOrElse(CommandResult())

  private def printResult(viewConfig: ViewConfig)(result: MashValue): CommandResult = {
    val printer = new Printer(output, terminalInfo, viewConfig)
    val PrintResult(printModelOpt) = printer.print(result)
    CommandResult(Some(result), printModelOpt = printModelOpt)
  }

  private def runProgram(program: AbstractSyntax.Program, unit: CompilationUnit): Option[MashValue] =
    try {
      val context = new ExecutionContext(Thread.currentThread)
      Singletons.environment = globals.get(StandardEnvironment.Env) match {
        case Some(obj: MashObject) ⇒ obj
        case _                     ⇒ MashObject.empty
      }
      Singletons.setExecutionContext(context)
      ExecutionContext.set(context)
      val result = Evaluator.evaluate(program.body)(EvaluationContext(ScopeStack(globals)))
      Some(result)
    } catch {
      case e@EvaluatorException(msg, stack, cause) ⇒
        if (printErrors)
          errorPrinter.printError("Error", msg, unit, stack.reverse)
        debugLogger.logException(e)
        None
      case _: EvaluationInterruptedException       ⇒
        val chars = "Interrupted:".style(foregroundColour = BasicColour.Yellow, bold = true) +
          " command cancelled by user".style(foregroundColour = BasicColour.Yellow)
        output.println(Screen.drawStyledChars(chars))
        None
    }

  private def safeCompile(unit: CompilationUnit, bareWords: Boolean, printErrors: Boolean = true): Option[AbstractSyntax.Program] = {
    val settings = CompilationSettings(bareWords = bareWords)
    Compiler.compile(unit, globals.immutableFields, settings) match {
      case Left(ParseError(msg, location)) ⇒
        if (printErrors)
          errorPrinter.printError("Syntax error", msg, unit, Seq(StackTraceItem(SourceLocation(unit.provenance, location))))
        None
      case Right(program)                  ⇒
        Some(program)
    }
  }

}