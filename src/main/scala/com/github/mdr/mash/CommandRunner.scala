package com.github.mdr.mash

import com.github.mdr.mash.evaluator._
import com.github.mdr.mash.lexer._
import com.github.mdr.mash.parser._
import com.github.mdr.mash.evaluator._
import com.github.mdr.mash.printer._
import com.github.mdr.mash.printer.Printer
import com.github.mdr.mash.terminal.TerminalInfo
import com.github.mdr.mash.compiler._
import com.github.mdr.mash.utils.Region
import com.github.mdr.mash.utils.PointedRegion
import org.fusesource.jansi.Ansi
import com.github.mdr.mash.utils.StringUtils
import java.io.PrintStream

case class CommandResult(value: Option[Any], toggleMish: Boolean = false)

object MishCommand {

  /**
   * Starts with a "!", but isn't a !{} expression
   */
  private val Regex = """^(\s*!)(?!\{)(.*)""".r

  def unapply(s: String): Option[(String, String)] =
    Regex.unapplySeq(s).map { case Seq(prefix, cmd) ⇒ (prefix, cmd) }
}

object DebugCommand {

  def unapply(s: String): Option[(String, String)] = {
    val trimmed = StringUtils.ltrim(s)
    if (trimmed startsWith ":") Some(trimmed.tail.span(_.isLetter)) else None
  }

}

class CommandRunner(output: PrintStream, terminalInfo: TerminalInfo, environment: Environment) {

  /**
   * @return the (optional) result of the command
   */
  def run(cmd: String, mish: Boolean = false, bareWords: Boolean): CommandResult = cmd match {
    case DebugCommand(keyword, args) ⇒
      runDebugCommand(keyword, args, bareWords)
      CommandResult(None)
    case MishCommand(prefix, mishCmd) ⇒
      if (mishCmd == "")
        CommandResult(None, toggleMish = true)
      else {
        runMishCommand(mishCmd, bareWords)
        CommandResult(None)
      }
    case _ if mish ⇒
      runMishCommand(cmd, bareWords)
      CommandResult(None)
    case _ ⇒ // regular mash
      val resultOpt = runMashCommand(cmd, bareWords)
      CommandResult(resultOpt)
  }

  private def runMashCommand(cmd: String, bareWords: Boolean): Option[Any] = {
    val exprOpt =
      try {
        Compiler.compile(cmd, environment, forgiving = false, inferTypes = false, bareWords = bareWords)
      } catch {
        case MashParserException(msg, location) ⇒
          printError("Syntax error", msg, cmd, Some(location))
          return None
        case MashLexerException(msg, location) ⇒
          printError("Syntax error", msg, cmd, Some(location))
          return None
      }
    for (expr ← exprOpt) yield {
      val result =
        try {
          val ctx = new ExecutionContext(Thread.currentThread)
          Singletons.setExecutionContext(ctx)
          ExecutionContext.set(ctx)
          Evaluator.evaluate(expr, environment)
        } catch {
          case e @ EvaluatorException(msg, locationOpt, cause) ⇒
            printError("Error", msg, cmd, locationOpt)
            DebugLogger.logException(e)
          case _: EvaluationInterruptedException ⇒
            output.println(Ansi.ansi().fg(Ansi.Color.YELLOW).bold.a("Interrupted:").boldOff.a(" command cancelled by user").reset())
        }
      val printer = new Printer(output, terminalInfo)
      printer.render(result)
      result
    }
  }

  private def printError(msgType: String, msg: String, cmd: String, regionOpt: Option[PointedRegion]) = {
    output.println(Ansi.ansi().fg(Ansi.Color.RED).bold.a(msgType + ":").boldOff.a(" " + msg).reset())
    output.println(Ansi.ansi().fg(Ansi.Color.RED).a(cmd).reset())
    for (PointedRegion(point, region @ Region(offset, length)) ← regionOpt) {
      output.print(Ansi.ansi().fg(Ansi.Color.RED))
      for (i ← 0 to region.posAfter)
        print(i match {
          case i if i == point         ⇒ "^"
          case i if region.contains(i) ⇒ "-"
          case _                       ⇒ " "
        })
      output.println(Ansi.ansi().reset())
    }

  }

  private def runMishCommand(cmd: String, bareWords: Boolean) {
    val exprOpt =
      try {
        Compiler.compile(cmd, environment, forgiving = false, inferTypes = false, mish = true, bareWords: Boolean)
      } catch {
        case MashParserException(msg, location) ⇒
          printError("Syntax error", msg, cmd, Some(location))
          return
        case MashLexerException(msg, location) ⇒
          printError("Syntax error", msg, cmd, Some(location))
          return
      }
    for (expr ← exprOpt) {
      val result =
        try {
          val ctx = new ExecutionContext(Thread.currentThread)
          Singletons.setExecutionContext(ctx)
          ExecutionContext.set(ctx)
          Evaluator.evaluate(expr, environment)
        } catch {
          case e @ EvaluatorException(msg, locationOpt, cause) ⇒
            printError("Error", msg, cmd, locationOpt)
            DebugLogger.logException(e)
          case _: EvaluationInterruptedException ⇒
            output.println(Ansi.ansi().fg(Ansi.Color.YELLOW).bold.a("Interrupted:").boldOff.a(" command cancelled by user").reset())
        }
    }
  }

  private def runDebugCommand(keyword: String, args: String, bareWords: Boolean) {
    (keyword, args) match {
      case ("p" | "pretty", actualCmd) ⇒
        for (expr ← Compiler.compile(actualCmd, environment, forgiving = true, inferTypes = true, bareWords = bareWords))
          TreePrettyPrinter.printTree(expr)
      case ("e" | "expression", actualCmd) ⇒
        for (expr ← Compiler.compile(actualCmd, environment, forgiving = true, bareWords = bareWords))
          println(PrettyPrinter.pretty(expr))
      case ("t" | "type", actualCmd) ⇒
        for (expr ← Compiler.compile(actualCmd, environment, forgiving = true, inferTypes = true, bareWords = bareWords))
          println(expr.typeOpt.getOrElse("Could not infer type"))
      case ("tokens", actualCmd) ⇒
        MashLexer.tokenise(actualCmd, forgiving = true, includeCommentsAndWhitespace = true).foreach(println)
    }
  }

}