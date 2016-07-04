package com.github.mdr.mash.commands

import java.io.PrintStream
import org.fusesource.jansi.Ansi
import com.github.mdr.mash.compiler.CompilationUnit
import com.github.mdr.mash.evaluator.SourceLocation
import com.github.mdr.mash.terminal.TerminalInfo
import com.github.mdr.mash.utils._

/**
 * Print errors with stack traces
 */
class ErrorPrinter(output: PrintStream, terminalInfo: TerminalInfo) {

  def printError(msgType: String, msg: String, unit: CompilationUnit, stack: Seq[SourceLocation]) = {
    output.println(formatStrong(msgType + ":") + formatRegular(" " + msg))
    if (stack.isEmpty)
      output.println(formatRegular(unit.text))
    else
      for (entry ← stack)
        printStackEntry(entry, unit)
  }

  private def printStackEntry(location: SourceLocation, unit: CompilationUnit) {
    val SourceLocation(provenance, PointedRegion(point, region)) = location
    val lineInfo = new LineInfo(provenance.source)
    val (lineIndex, _) = lineInfo.lineAndColumn(point)
    val line = lineInfo.lines(lineIndex)
    val isImmediateError = unit.provenance == provenance && unit.interactive
    val prefix = if (isImmediateError) "" else s"${provenance.name}:${lineIndex + 1}: "
    val padding = " " * prefix.length
    val lineRegion = lineInfo.lineRegion(lineIndex)
    val errorUnderlineLine = getUnderlineLine(prefix, lineInfo, lineIndex, point, region)
    output.println(formatStrong(prefix) + formatRegular(line))
    output.println(formatRegular(errorUnderlineLine))
  }

  private def getUnderlineLine(prefix: String, lineInfo: LineInfo, lineIndex: Int, point: Int, region: Region): String = {
    val padding = " " * prefix.length
    val lineRegion = lineInfo.lineRegion(lineIndex)
    padding +
      (for (i ← lineRegion.range)
        yield i match {
        case i if i == point        ⇒ "^"
        case i if region contains i ⇒ "-"
        case _                      ⇒ " "
      }).mkString
  }

  private def formatStrong(s: String): String = Ansi.ansi.fg(Ansi.Color.RED).bold.a(s).reset.toString
  private def formatRegular(s: String) = Ansi.ansi.fg(Ansi.Color.RED).a(s).reset.toString

}