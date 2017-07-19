package com.github.mdr.mash.screen

import com.github.mdr.mash.evaluator.TildeExpander
import com.github.mdr.mash.os.linux.{ LinuxEnvironmentInteractions, LinuxFileSystem }
import com.github.mdr.mash.repl.ReplState
import com.github.mdr.mash.runtime.MashObject
import com.github.mdr.mash.screen.Style.StylableString
import com.github.mdr.mash.utils.{ Dimensions, LineInfo, Point }

object LineBufferRenderer {

  private val envInteractions = LinuxEnvironmentInteractions
  private val fileSystem = LinuxFileSystem

  def renderLineBuffer(state: ReplState, terminalSize: Dimensions, globalVariables: MashObject, bareWords: Boolean): LinesAndCursorPos = {
    val prompt = getPrompt(state.commandNumber, state.mish)
    val lineBuffer = state.lineBuffer
    val cursorPos = lineBuffer.cursorPos
    val unwrappedLines = renderLineBufferChars(lineBuffer.text, lineBuffer.cursorOffset, prompt,
      state.mish, globalVariables, bareWords)

    def wrap(line: Line): Seq[Line] = {
      val groups = line.string.grouped(terminalSize.columns).toSeq
      for {
        (group, index) ← groups.zipWithIndex
        endsInNewline = index == groups.size - 1
      } yield Line(group, endsInNewline)
    }

    val wrappedLines = unwrappedLines.flatMap(wrap)
    val row = unwrappedLines.take(cursorPos.row).flatMap(wrap).length + (prompt.length + cursorPos.column) / terminalSize.columns
    val column = (prompt.length + cursorPos.column) % terminalSize.columns
    LinesAndCursorPos(wrappedLines, Some(Point(row, column)))
  }

  private def renderLineBufferChars(rawChars: String,
                            cursorOffset: Int,
                            prompt: StyledString,
                            mishByDefault: Boolean,
                            globalVariables: MashObject,
                            bareWords: Boolean): Seq[Line] = {
    val mashRenderer = new MashRenderer(Some(globalVariables), bareWords)
    val renderedMash: StyledString = mashRenderer.renderChars(rawChars, Some(cursorOffset), mishByDefault)
    val continuationPrefix = if (prompt.isEmpty) "" else "." * (prompt.length - 1) + " "
    val lineRegions = new LineInfo(rawChars).lineRegions
    lineRegions.zipWithIndex.map {
      case (region, 0) ⇒ Line(prompt + region.of(renderedMash.chars))
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