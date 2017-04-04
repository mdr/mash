package com.github.mdr.mash.commands

import java.io.PrintStream

import com.github.mdr.mash.compiler.CompilationUnit
import com.github.mdr.mash.evaluator.{ SourceLocation, StackTraceItem, TildeExpander }
import com.github.mdr.mash.os.linux.LinuxEnvironmentInteractions
import com.github.mdr.mash.screen.Screen.drawStyledChars
import com.github.mdr.mash.screen.Style.StylableString
import com.github.mdr.mash.screen.{ BasicColour, MashRenderer }
import com.github.mdr.mash.terminal.TerminalInfo
import com.github.mdr.mash.utils._

/**
  * Print errors with stack traces
  */
class ErrorPrinter(output: PrintStream, terminalInfo: TerminalInfo) {

  private val envInteractions = LinuxEnvironmentInteractions

  def printError(msgType: String, msg: String, unit: CompilationUnit, stack: Seq[StackTraceItem]) = {
    output.println(formatStrong(msgType + ":") + " " + msg)
    if (stack.isEmpty)
      output.println(formatRegular(unit.text))
    else
      for (entry ← stack)
        printStackEntry(entry, unit)
  }

  private def printStackEntry(item: StackTraceItem, unit: CompilationUnit) {
    val StackTraceItem(SourceLocation(provenance, PointedRegion(point, region)), functionOpt) = item
    val lineInfo = new LineInfo(provenance.source)
    val (lineIndex, _) = lineInfo.lineAndColumn(point)
    val line = lineInfo.lines(lineIndex)
    val renderedLine = new MashRenderer().renderChars(line, cursorOffset = line.length, mishByDefault = false)
    val drawnLine = drawStyledChars(renderedLine)
    val isImmediateError = unit.provenance == provenance && unit.interactive
    val functionName = functionOpt.map(f ⇒ ":" + f.name).getOrElse("")
    val prefix = if (isImmediateError) "" else s"${replaceHomePath(provenance.name)}:${lineIndex + 1}$functionName: "
    val errorUnderlineLine = getUnderlineLine(prefix, lineInfo, lineIndex, point, region)
    output.println(formatStrong(prefix) + drawnLine)
    output.println(formatStrong(errorUnderlineLine))
  }

  private def replaceHomePath(path: String): String =
    new TildeExpander(envInteractions).retilde(path)

  private def getUnderlineLine(prefix: String, lineInfo: LineInfo, lineIndex: Int, point: Int, region: Region): String = {
    val padding = " " * prefix.length
    val lineRegion = lineInfo.lineRegion(lineIndex)
    padding + lineRegion.range.map {
      case i if i == point        ⇒ "^"
      case i if region contains i ⇒ "-"
      case _                      ⇒ " "
    }.mkString
  }

  private def formatStrong(s: String): String =
    drawStyledChars(s.style(foregroundColour = BasicColour.Red, bold = true))

  private def formatRegular(s: String) =
    drawStyledChars(s.style(foregroundColour = BasicColour.Red))

}