package com.github.mdr.mash.render

import com.github.mdr.mash.evaluator.TildeExpander
import com.github.mdr.mash.os.linux.{ LinuxEnvironmentInteractions, LinuxFileSystem }
import com.github.mdr.mash.repl.{ LineBuffer, ReplState }
import com.github.mdr.mash.runtime.MashObject
import com.github.mdr.mash.screen.Style.StylableString
import com.github.mdr.mash.screen.{ BasicColour, _ }
import com.github.mdr.mash.utils.{ Dimensions, LineInfo, Point }

object LineBufferRenderer {

  private val envInteractions = LinuxEnvironmentInteractions
  private val fileSystem = LinuxFileSystem

  def renderLineBuffer(state: ReplState,
                       terminalSize: Dimensions,
                       globalVariables: MashObject,
                       bareWords: Boolean): LinesAndCursorPos = {
    val prefix = getPrompt(state.commandNumber, state.mish)
    renderLineBuffer(state.lineBuffer, Some(globalVariables), prefix, bareWords, state.mish, terminalSize)
  }

  def renderLineBuffer(lineBuffer: LineBuffer,
                       globalVariablesOpt: Option[MashObject],
                       prefix: StyledString,
                       bareWords: Boolean,
                       mish: Boolean,
                       terminalSize: Dimensions): LinesAndCursorPos = {
    val unwrappedLines = renderLineBufferChars(lineBuffer.text, lineBuffer.cursorOffset, prefix, mish,
      globalVariablesOpt, bareWords)

    val wrappedLines = unwrappedLines.flatMap(wrap(_, terminalSize.columns))

    val cursorPos = lineBuffer.cursorPos
    val row = unwrappedLines.take(cursorPos.row).flatMap(wrap(_, terminalSize.columns)).length +
      (prefix.length + cursorPos.column) / terminalSize.columns
    val column = (prefix.length + cursorPos.column) % terminalSize.columns
    val wrappedCursorPos = Point(row, column)

    LinesAndCursorPos(wrappedLines, Some(wrappedCursorPos))
  }

  private def wrap(line: Line, columns: Int): Seq[Line] = {
    val groups = line.string.grouped(columns).toSeq
    for {
      (group, index) ← groups.zipWithIndex
      endsInNewline = index == groups.size - 1
    } yield Line(group, endsInNewline)
  }

  private def renderLineBufferChars(rawChars: String,
                                    cursorOffset: Int,
                                    prefix: StyledString,
                                    mishByDefault: Boolean,
                                    globalVariablesOpt: Option[MashObject],
                                    bareWords: Boolean): Seq[Line] = {
    val mashRenderer = new MashRenderer(globalVariablesOpt, bareWords)
    val renderedMash: StyledString = mashRenderer.renderChars(rawChars, Some(cursorOffset), mishByDefault)
    val continuationPrefix = if (prefix.isEmpty) "" else "." * (prefix.length - 1) + " "
    val lineRegions = new LineInfo(rawChars).lineRegions
    lineRegions.zipWithIndex.map {
      case (region, 0) ⇒ Line(prefix + region.of(renderedMash.chars))
      case (region, _) ⇒ Line(continuationPrefix.style + region.of(renderedMash.chars))
    }
  }

  private def getPrompt(commandNumber: Int, mishByDefault: Boolean): StyledString = {
    val num = s"[$commandNumber] "
    val numStyle = Style(foregroundColour = BasicColour.Yellow)
    val numStyled = num.style(numStyle)

    val pwd = new TildeExpander(envInteractions).retilde(fileSystem.pwd.toString)
    val pwdStyle = Style(foregroundColour = BasicColour.Cyan, bold = true)
    val pwdStyled = pwd.style(pwdStyle)

    val promptChar = if (mishByDefault) "!" else "$"
    val promptCharStyle = Style(foregroundColour = BasicColour.Green, bold = true)
    val promptCharStyled = s" $promptChar ".style(promptCharStyle)

    numStyled + pwdStyled + promptCharStyled
  }

}