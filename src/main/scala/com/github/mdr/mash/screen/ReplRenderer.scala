package com.github.mdr.mash.screen

import com.github.mdr.mash.assist.AssistanceState
import com.github.mdr.mash.os.linux.LinuxFileSystem
import com.github.mdr.mash.repl._
import com.github.mdr.mash.repl.browser._
import com.github.mdr.mash.repl.history.HistorySearchState
import com.github.mdr.mash.screen.Style.StylableString
import com.github.mdr.mash.screen.browser._
import com.github.mdr.mash.utils.{ Dimension, Point }

case class LinesAndCursorPos(lines: Seq[Line], cursorPos: Point)

/**
  * Render the current state (input buffer, completion state, assistance information etc) into a set of lines of styled
  * characters.
  */
object ReplRenderer {

  private val fileSystem = LinuxFileSystem

  def render(state: ReplState, terminalSize: Dimension): Screen =
    state.objectBrowserStateStackOpt match {
      case Some(objectBrowserState) ⇒ renderObjectBrowser(objectBrowserState, terminalSize)
      case None                     ⇒ renderRegularRepl(state, terminalSize)
    }

  private def renderRegularRepl(state: ReplState, terminalSize: Dimension): Screen = {
    val LinesAndCursorPos(bufferLines, bufferCursorPos) = LineBufferRenderer.renderLineBuffer(state, terminalSize)
    val historySearchScreenOpt = state.historySearchStateOpt.map(renderHistorySearchState(_, terminalSize))
    val historySearchLines = historySearchScreenOpt.map(_.lines).getOrElse(Seq())
    val assistanceLines = renderAssistanceState(state.assistanceStateOpt, terminalSize)
    val remainingRows = math.max(0, terminalSize.rows - bufferLines.size - assistanceLines.size - historySearchLines.size)
    val remainingSpace = terminalSize.copy(rows = remainingRows)
    val CompletionRenderResult(completionLines) =
      CompletionRenderer.renderCompletions(state.completionStateOpt, remainingSpace)
    val lines = bufferLines ++ historySearchLines ++ completionLines ++ assistanceLines
    val truncatedLines = lines.take(terminalSize.rows)
    val newCursorPos = historySearchScreenOpt.map(_.cursorPos.down(bufferLines.size)).getOrElse(bufferCursorPos)
    val title = fileSystem.pwd.toString
    Screen(truncatedLines, Some(newCursorPos), title)
  }

  private def renderObjectTableBrowser(state: TwoDTableBrowserState, terminalSize: Dimension): Screen =
    new TwoDTableBrowserRenderer(state, terminalSize).renderObjectBrowser

  private def renderSingleObjectBrowser(state: SingleObjectTableBrowserState, terminalSize: Dimension): Screen =
    new SingleObjectTableBrowserRenderer(state, terminalSize).renderObjectBrowser

  private def renderObjectTreeBrowser(state: ObjectTreeBrowserState, terminalSize: Dimension): Screen =
    new ObjectTreeBrowserRenderer(state, terminalSize).renderObjectBrowser

  private def renderValueBrowser(state: ValueBrowserState, terminalSize: Dimension): Screen =
    new ValueBrowserRenderer(state, terminalSize).renderObjectBrowser

  private def renderTextLinesBrowserState(state: TextLinesBrowserState, terminalSize: Dimension): Screen =
    new TextLinesBrowserRenderer(state, terminalSize).renderObjectBrowser

  private def renderObjectBrowser(state: ObjectBrowserStateStack, terminalSize: Dimension): Screen =
    state.headState match {
      case browserState: TwoDTableBrowserState         ⇒ renderObjectTableBrowser(browserState, terminalSize)
      case browserState: SingleObjectTableBrowserState ⇒ renderSingleObjectBrowser(browserState, terminalSize)
      case browserState: ObjectTreeBrowserState        ⇒ renderObjectTreeBrowser(browserState, terminalSize)
      case browserState: ValueBrowserState             ⇒ renderValueBrowser(browserState, terminalSize)
      case browserState: TextLinesBrowserState         ⇒ renderTextLinesBrowserState(browserState, terminalSize)
      case browserState                                ⇒ throw new RuntimeException(s"Unknown browser state of type: ${state.getClass.getSimpleName}")
    }

  private def renderHistorySearchState(searchState: HistorySearchState, terminalSize: Dimension): LinesAndCursorPos = {
    val prefixChars: StyledString = "Incremental history search: ".style
    val searchChars: StyledString = searchState.searchString.style(Style(foregroundColour = BasicColour.Cyan))
    val chars = (prefixChars + searchChars).take(terminalSize.columns)
    val line = Line((prefixChars + searchChars).take(terminalSize.columns))
    val cursorPos = Point(0, chars.size)
    LinesAndCursorPos(Seq(line), cursorPos)
  }

  private def renderAssistanceState(assistanceStateOpt: Option[AssistanceState], terminalSize: Dimension): Seq[Line] =
    assistanceStateOpt.toSeq.flatMap(AssistanceRenderer.render(_, terminalSize))

}