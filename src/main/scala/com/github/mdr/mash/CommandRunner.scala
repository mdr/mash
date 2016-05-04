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

case class CommandResult(
  value: Option[Any] = None,
  toggleMish: Boolean = false,
  insertReferenceOpt: Option[Int] = None)

object MishCommand {

  /**
   * Starts with a "!", but isn't a !{} or !!{} expression
   */
  private val MishRegex = """^(\s*!)(?!\{|!\{)(.*)""".r

  def unapply(s: String): Option[(String, String)] =
    MishRegex.unapplySeq(s).map { case Seq(prefix, cmd) ⇒ (prefix, cmd) }

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
  def run(cmd: String, mish: Boolean = false, bareWords: Boolean): CommandResult =
    cmd match {
      case DebugCommand(keyword, args) ⇒
        runDebugCommand(keyword, args, bareWords)
        CommandResult()
      case MishCommand(prefix, mishCmd) ⇒
        if (mishCmd == "")
          CommandResult(toggleMish = true)
        else
          runCommand(mishCmd, mish = true, bareWords = bareWords)
      case _ if mish ⇒
        runCommand(cmd, mish = true, bareWords = bareWords)
      case _ ⇒ // regular mash
        runCommand(cmd, mish = false, bareWords = bareWords)
    }

  private def runCommand(cmd: String, bareWords: Boolean, mish: Boolean): CommandResult = {
    val exprOpt = safeCompile(cmd, bareWords = bareWords, mish = mish)
    exprOpt.map { expr ⇒
      val result = runExpr(expr, cmd)
      val printer = new Printer(output, terminalInfo)
      val PrintResult(insertReferenceOpt) = printer.render(result)
      CommandResult(Some(result), insertReferenceOpt = insertReferenceOpt)
    }.getOrElse(CommandResult())
  }

  private def runExpr(expr: AbstractSyntax.Expr, cmd: String): Any =
    try {
      val ctx = new ExecutionContext(Thread.currentThread)
      Singletons.setExecutionContext(ctx)
      ExecutionContext.set(ctx)
      Evaluator.evaluate(expr, environment)
    } catch {
      case e @ EvaluatorException(msg, locationOpt, cause) ⇒
        printError("Error", msg, cmd, locationOpt)
        DebugLogger.logException(e)
        ()
      case _: EvaluationInterruptedException ⇒
        output.println(Ansi.ansi().fg(Ansi.Color.YELLOW).bold.a("Interrupted:").boldOff.a(" command cancelled by user").reset())
        ()
    }

  private def safeCompile(cmd: String, bareWords: Boolean, mish: Boolean): Option[AbstractSyntax.Expr] =
    try
      Compiler.compile(cmd, environment, forgiving = false, inferTypes = false, mish = mish, bareWords = bareWords)
    catch {
      case MashParserException(msg, location) ⇒
        printError("Syntax error", msg, cmd, Some(location))
        None
      case MashLexerException(msg, location) ⇒
        printError("Syntax error", msg, cmd, Some(location))
        None
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

  private def runDebugCommand(keyword: String, args: String, bareWords: Boolean) {
    (keyword, args) match {
      case ("p" | "pretty", actualCmd) ⇒
        for (expr ← Compiler.compile(actualCmd, environment, forgiving = true, inferTypes = true, bareWords = bareWords))
          TreePrettyPrinter.printTree(expr)
      case ("e" | "expression", actualCmd) ⇒
        for (expr ← Compiler.compile(actualCmd, environment, forgiving = true, bareWords = bareWords))
          output.println(PrettyPrinter.pretty(expr))
      case ("t" | "type", actualCmd) ⇒
        for (expr ← Compiler.compile(actualCmd, environment, forgiving = true, inferTypes = true, bareWords = bareWords))
          output.println(expr.typeOpt.getOrElse("Could not infer type"))
      case ("tokens", actualCmd) ⇒
        MashLexer.tokenise(actualCmd, forgiving = true, includeCommentsAndWhitespace = true).foreach(output.println)
    }
  }

}