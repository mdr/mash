package com.github.mdr.mash.view.render

import com.github.mdr.mash.compiler.BareStringify
import com.github.mdr.mash.evaluator.TildeExpander
import com.github.mdr.mash.os.{ EnvironmentInteractions, FileSystem }
import com.github.mdr.mash.parser.{ Abstractifier, MashParser, Provenance }
import com.github.mdr.mash.repl.{ LineBuffer, ReplState }
import com.github.mdr.mash.runtime.MashString
import com.github.mdr.mash.screen.Style._
import com.github.mdr.mash.screen._
import com.github.mdr.mash.utils.Utils._
import com.github.mdr.mash.utils.{ Dimensions, Point, Region }

class LineBufferRenderer(envInteractions: EnvironmentInteractions, fileSystem: FileSystem) {

  def renderLineBuffer(state: ReplState,
                       terminalSize: Dimensions,
                       context: MashRenderingContext,
                       completed: Boolean): LinesAndCursorPos = {
    val prefix = renderPrompt(state.commandNumber, state.mish)
    val matchRegionOpt = state.incrementalHistorySearchStateOpt.flatMap(_.hitStatus.matchRegionOpt)
    renderLineBuffer(state.lineBuffer, terminalSize, context, prefix, matchRegionOpt, completed)
  }

  def renderLineBuffer(lineBuffer: LineBuffer,
                       terminalSize: Dimensions,
                       context: MashRenderingContext,
                       prefix: StyledString = StyledString.Empty,
                       matchRegionOpt: Option[Region] = None,
                       completed: Boolean = false): LinesAndCursorPos = {
    val unwrappedLines = renderWithoutWrapping(lineBuffer, prefix, context, matchRegionOpt)

    val wrappedLines = unwrappedLines.flatMap { case UnwrappedRenderedLine(line, selectionWraps) ⇒
      wrap(line, terminalSize.columns, selectionWraps)
    }

    val cursorPos = lineBuffer.cursorPos
    val row = unwrappedLines.take(cursorPos.row).flatMap { case UnwrappedRenderedLine(line, _) ⇒ wrap(line, terminalSize.columns) }.length +
      (prefix.length + cursorPos.column) / terminalSize.columns
    val column = (prefix.length + cursorPos.column) % terminalSize.columns
    val wrappedCursorPos = Point(row, column)

    val lines = wrappedLines.when(!completed, addStatusIndicator(lineBuffer, context, terminalSize))

    LinesAndCursorPos(lines, Some(wrappedCursorPos))
  }

  private def addStatusIndicator(lineBuffer: LineBuffer, context: MashRenderingContext, terminalSize: Dimensions)(wrappedLines: Seq[Line]) = {
    val lastLine = wrappedLines.last.string.padTo(terminalSize.columns, StyledCharacter(' '))
    val hasError = MashParser.parse(lineBuffer.text, context.mishByDefault) match {
      case Left(_)        ⇒ true
      case Right(program) ⇒
        !context.bareWords && context.globalVariablesOpt.exists { globalVariables ⇒
          val bindings = globalVariables.immutableFields.keySet.collect { case s: MashString ⇒ s.s }
          val provenance = Provenance.internal(lineBuffer.text)
          val abstractExpr = new Abstractifier(provenance).abstractify(program).body

          BareStringify.getBareTokens(abstractExpr, bindings).nonEmpty
        }
    }

    val indicatorStyle = if (hasError) Style(foregroundColour = DefaultColours.Red) else Style(DefaultColours.Green)
    val errorIndicatorStyled = '◉'.style(indicatorStyle)
    val newLastLine = lastLine.updated(lastLine.length - 1, errorIndicatorStyled)
    wrappedLines.init :+ Line(newLastLine)
  }

  private def wrap(line: Line, columns: Int, selectionWraps: Boolean = false): Seq[Line] = {
    val groupedGroups = if (line.isEmpty) Seq(StyledString.Empty) else line.string.grouped(columns).toSeq
    val groups = groupedGroups.when(groupedGroups.last.chars.length == columns, _ :+ StyledString.Empty)
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
        .restyle(lineBuffer.selectedOrCursorRegion, Style(inverse = true))

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
    val numStyle = Style(foregroundColour = DefaultColours.Yellow)
    val numStyled = num.style(numStyle)

    val pwd = new TildeExpander(envInteractions).retilde(fileSystem.pwd.toString)
    val pwdStyle = Style(foregroundColour = DefaultColours.Cyan)
    val pwdStyled = pwd.style(pwdStyle)

    val promptChar = if (mishByDefault) "!" else "$"
    val promptCharStyle = Style(foregroundColour = DefaultColours.Green, bold = true)
    val promptCharStyled = s" $promptChar ".style(promptCharStyle)

    numStyled + pwdStyled + promptCharStyled
  }

}