package com.github.mdr.mash

import com.github.mdr.mash.runtime._
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
import scala.collection.mutable
import scala.collection.immutable.ListMap

case class CommandResult(
  value: Option[MashValue] = None,
  toggleMish: Boolean = false,
  objectTableModelOpt: Option[ObjectTableModel] = None)

object MishCommand {

  /**
   * Starts with a "!", but isn't a !{} or !!{} expression
   */
  private val MishRegex = """^(\s*!)(?!\{|!\{)(.*)""".r

  def unapply(s: String): Option[(String, String)] =
    MishRegex.unapplySeq(s).map { case Seq(prefix, cmd) ⇒ (prefix, cmd) }

}

object SuffixMishCommand {

  def unapply(s: String): Option[(String, String)] =
    if (s endsWith "!") Some(s.dropRight(1) -> s.takeRight(1)) else None

}

object DebugCommand {

  def unapply(s: String): Option[(String, String)] = {
    val trimmed = StringUtils.ltrim(s)
    if (trimmed startsWith ":") Some(trimmed.tail.span(_.isLetter)) else None
  }

}

class CommandRunner(output: PrintStream, terminalInfo: TerminalInfo, globalVariables: MashObject) {

  /**
   * @return the (optional) result of the command
   */
  def run(cmd: String, unitName: String, mish: Boolean = false, bareWords: Boolean): CommandResult = {
    cmd match {
      case DebugCommand(keyword, args) ⇒
        runDebugCommand(keyword, args, bareWords)
        CommandResult()
      case MishCommand(prefix, mishCmd) ⇒
        if (mishCmd == "")
          CommandResult(toggleMish = true)
        else {
          val unit = CompilationUnit(mishCmd, unitName)
          runCommand(unit, mish = true, bareWords = bareWords)
        }
      case SuffixMishCommand(mishCmd, suffix) ⇒
        val unit = CompilationUnit(mishCmd, unitName)
        runCommand(unit, mish = true, bareWords = bareWords)
      case _ if mish ⇒
        val unit = CompilationUnit(cmd, unitName)
        runCommand(unit, mish = true, bareWords = bareWords)
      case _ ⇒ // regular mash
        val unit = CompilationUnit(cmd, unitName)
        runCommand(unit, mish = false, bareWords = bareWords)
    }
  }

  private def runCommand(unit: CompilationUnit, bareWords: Boolean, mish: Boolean): CommandResult = {
    safeCompile(unit, bareWords = bareWords, mish = mish).map { expr ⇒
      val result = runExpr(expr, unit)
      val printer = new Printer(output, terminalInfo)
      val PrintResult(objectTableModelOpt) = printer.print(result)
      CommandResult(Some(result), objectTableModelOpt = objectTableModelOpt)
    }.getOrElse(CommandResult())
  }

  private def runExpr(expr: AbstractSyntax.Expr, unit: CompilationUnit): MashValue =
    try {
      val ctx = new ExecutionContext(Thread.currentThread)
      Singletons.environment = globalVariables.getField("env") match {
        case Some(mo: MashObject) ⇒ mo
        case _                    ⇒ MashObject(ListMap[String, MashValue](), classOpt = None)
      }
      Singletons.setExecutionContext(ctx)
      ExecutionContext.set(ctx)
      Evaluator.evaluate(expr)(EvaluationContext(ScopeStack(globalVariables.fields)))
    } catch {
      case e @ EvaluatorException(msg, stack, cause) ⇒
        printError("Error", msg, unit, stack.reverse)
        DebugLogger.logException(e)
        MashUnit
      case _: EvaluationInterruptedException ⇒
        output.println(Ansi.ansi().fg(Ansi.Color.YELLOW).bold.a("Interrupted:").boldOff.a(" command cancelled by user").reset())
        MashUnit
    }

  private def safeCompile(unit: CompilationUnit, bareWords: Boolean, mish: Boolean): Option[AbstractSyntax.Expr] =
    try
      Compiler.compile(unit, globalVariables.immutableFields, forgiving = false, inferTypes = false, mish = mish, bareWords = bareWords)
    catch {
      case MashParserException(msg, location) ⇒
        printError("Syntax error", msg, unit, Seq(SourceLocation(unit.provenance, location)))
        None
      case MashLexerException(msg, location) ⇒
        printError("Syntax error", msg, unit, Seq(SourceLocation(unit.provenance, location)))
        None
    }

  private def printError(msgType: String, msg: String, unit: CompilationUnit, stack: Seq[SourceLocation]) = {
    output.println(Ansi.ansi().fg(Ansi.Color.RED).bold.a(msgType + ":").boldOff.a(" " + msg).reset())
    if (stack.isEmpty)
      output.println(Ansi.ansi().fg(Ansi.Color.RED).a(unit.text).reset())
    else {
      for (entry ← stack) {
        val SourceLocation(provenance, PointedRegion(point, region @ Region(offset, length))) = entry
        if (unit.provenance == provenance)
          output.println(Ansi.ansi().fg(Ansi.Color.RED).a(provenance.source).reset())
        else {
          output.print(Ansi.ansi().bold.fg(Ansi.Color.RED).a(provenance.name).reset())
          output.println(Ansi.ansi().fg(Ansi.Color.RED).a(": " + provenance.source).reset())
        }
        output.print(Ansi.ansi().fg(Ansi.Color.RED))
        val padding = if (unit.provenance == provenance) "" else " " * (provenance.name.length + ": ".length)
        output.print(padding)
        for (i ← 0 to region.posAfter)
          output.print(i match {
            case i if i == point         ⇒ "^"
            case i if region.contains(i) ⇒ "-"
            case _                       ⇒ " "
          })
        output.println(Ansi.ansi().reset())
      }
    }
  }

  private def runDebugCommand(keyword: String, args: String, bareWords: Boolean) {
    (keyword, args) match {
      case ("p" | "pretty", actualCmd) ⇒
        for (expr ← Compiler.compile(CompilationUnit(actualCmd), globalVariables.immutableFields, forgiving = true, inferTypes = true, bareWords = bareWords))
          TreePrettyPrinter.printTree(expr)
      case ("e" | "expression", actualCmd) ⇒
        for (expr ← Compiler.compile(CompilationUnit(actualCmd), globalVariables.immutableFields, forgiving = true, bareWords = bareWords))
          output.println(PrettyPrinter.pretty(expr))
      case ("t" | "type", actualCmd) ⇒
        for (expr ← Compiler.compile(CompilationUnit(actualCmd), globalVariables.immutableFields, forgiving = true, inferTypes = true, bareWords = bareWords))
          output.println(expr.typeOpt.getOrElse("Could not infer type"))
      case ("tokens", actualCmd) ⇒
        MashLexer.tokenise(actualCmd, forgiving = true, includeCommentsAndWhitespace = true).foreach(output.println)
    }
  }

}