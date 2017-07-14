package com.github.mdr.mash.commands

import java.io.PrintStream

import com.github.mdr.mash.compiler.CompilationUnit
import com.github.mdr.mash.evaluator.{ SourceLocation, StackTraceItem, TildeExpander }
import com.github.mdr.mash.os.linux.LinuxEnvironmentInteractions
import com.github.mdr.mash.screen.Screen.drawStyledChars
import com.github.mdr.mash.screen.Style.StylableString
import com.github.mdr.mash.screen.{ BasicColour, MashRenderer, Point }
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
    val StackTraceItem(sourceLocationOpt, functionOpt) = item

    sourceLocationOpt match {
      case Some(SourceLocation(provenance, PointedRegion(point, region))) ⇒
        val lineInfo = new LineInfo(provenance.source)
        val Point(pointLineIndex, _) = lineInfo.lineAndColumn(point)
        val isImmediateError = unit.provenance == provenance && unit.interactive
        val functionName = functionOpt.map(f ⇒ ":" + f.name).getOrElse("")
        val prefix = if (isImmediateError) "" else s"${replaceHomePath(provenance.name)}:${pointLineIndex + 1}$functionName: "

        val (firstLineIndex, lastLineIndex) = lineInfo.linesOfRegion(region)
        for (lineIndex ← firstLineIndex to lastLineIndex) {
          val line = lineInfo.lines(lineIndex)
          val renderedLine = new MashRenderer().renderChars(line, cursorOffsetOpt = None, mishByDefault = false)
          val drawnLine = drawStyledChars(renderedLine)
          val errorUnderlineLine = getUnderlineLine(prefix, lineInfo, lineIndex, point, region)
          val actualPrefix = if (lineIndex == firstLineIndex) prefix else " " * prefix.length
          output.println(formatStrong(actualPrefix) + drawnLine)
          output.println(formatStrong(errorUnderlineLine))
        }
      case None ⇒
        val functionName = functionOpt.map(_.name).getOrElse("anonymous")
        output.println(formatStrong(s"builtin:$functionName: ") + "no associated source")
        output.println()
    }
  }

  private def replaceHomePath(path: String): String =
    new TildeExpander(envInteractions).retilde(path)

  private def getUnderlineLine(prefix: String, lineInfo: LineInfo, lineIndex: Int, point: Int, region: Region): String = {
    val padding = " " * prefix.length
    val lineRegion = lineInfo.lineRegion(lineIndex)
    val mainLine = lineRegion.range.map {
      case i if i == point        ⇒ "^"
      case i if region contains i ⇒ "-"
      case _                      ⇒ " "
    }
    val endOfLinePoint = if (point == lineRegion.posAfter) "^" else ""
    padding + mainLine.mkString + endOfLinePoint
  }

  private def formatStrong(s: String): String =
    drawStyledChars(s.style(foregroundColour = BasicColour.Red, bold = true))

  private def formatRegular(s: String) =
    drawStyledChars(s.style(foregroundColour = BasicColour.Red))

}