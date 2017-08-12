package com.github.mdr.mash.render

import com.github.mdr.mash.evaluator.TildeExpander
import com.github.mdr.mash.os.linux.{ LinuxEnvironmentInteractions, LinuxFileSystem }
import com.github.mdr.mash.repl.{ LineBuffer, ReplState }
import com.github.mdr.mash.screen.Style.StylableString
import com.github.mdr.mash.screen.{ BasicColour, _ }
import com.github.mdr.mash.utils.Utils._
import com.github.mdr.mash.utils.{ Dimensions, Point, Region }

object LineBufferRenderer {

  private val envInteractions = LinuxEnvironmentInteractions
  private val fileSystem = LinuxFileSystem

  def renderLineBuffer(state: ReplState,
                       terminalSize: Dimensions,
                       context: MashRenderingContext): LinesAndCursorPos = {
    val prefix = renderPrompt(state.commandNumber, state.mish)
    val matchRegionOpt = state.incrementalHistorySearchStateOpt.flatMap(_.hitStatus.matchRegionOpt)
    renderLineBuffer(state.lineBuffer, context, prefix, terminalSize, matchRegionOpt)
  }

  def renderLineBuffer(lineBuffer: LineBuffer,
                       context: MashRenderingContext,
                       prefix: StyledString = StyledString.Empty,
                       terminalSize: Dimensions,
                       matchRegionOpt: Option[Region] = None): LinesAndCursorPos = {
    val unwrappedLines = renderWithoutWrapping(lineBuffer, prefix, context, matchRegionOpt)

    val wrappedLines = unwrappedLines.flatMap { case UnwrappedRenderedLine(line, selectionWraps) ⇒
      wrap(line, terminalSize.columns, selectionWraps)
    }

    val cursorPos = lineBuffer.cursorPos
    val row = unwrappedLines.take(cursorPos.row).flatMap { case UnwrappedRenderedLine(line, _) ⇒ wrap(line, terminalSize.columns) }.length +
      (prefix.length + cursorPos.column) / terminalSize.columns
    val column = (prefix.length + cursorPos.column) % terminalSize.columns
    val wrappedCursorPos = Point(row, column)

    LinesAndCursorPos(wrappedLines, Some(wrappedCursorPos))
  }

  private def wrap(line: Line, columns: Int, selectionWraps: Boolean = false): Seq[Line] = {
    val groups = line.string.grouped(columns).toSeq
    for {
      (group, index) ← groups.zipWithIndex
      isLastGroup = index == groups.size - 1
      lineContents = group.when(isLastGroup && selectionWraps, _.padTo(columns, StyledCharacter(' ', Style(inverse = true))))
    } yield Line(lineContents, endsInNewline = isLastGroup)
  }

  private case class UnwrappedRenderedLine(line: Line, selectionWraps: Boolean)

  private def renderWithoutWrapping(lineBuffer: LineBuffer,
                                    prefix: StyledString,
                                    context: MashRenderingContext,
                                    matchRegionOpt: Option[Region]): Seq[UnwrappedRenderedLine] = {
    val rawChars = lineBuffer.text
    val cursorOffset = lineBuffer.cursorOffset
    val mashRenderer = new MashRenderer(context)
    val renderedMash: StyledString =
      mashRenderer.renderChars(rawChars, Some(cursorOffset), matchRegionOpt)
        .invert(lineBuffer.selectedOrCursorRegion)

    val continuationPrefix = if (prefix.isEmpty) "" else "." * (prefix.length - 1) + " "
    val lineInfo = lineBuffer.lineInfo
    lineInfo.lineRegions.zipWithIndex.map { case (region, i) ⇒
      val linePrefix = if (i == 0) prefix else continuationPrefix.style
      val line = Line(linePrefix + region.of(renderedMash.chars))
      val selectionWraps = lineBuffer.selectedOrCursorRegion.contains(lineInfo.lineEnd(i) - 1) && i != lineInfo.lineCount - 1
      UnwrappedRenderedLine(line, selectionWraps)
    }
  }

  private def renderPrompt(commandNumber: Int, mishByDefault: Boolean): StyledString = {
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