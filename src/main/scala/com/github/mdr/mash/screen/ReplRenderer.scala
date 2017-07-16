package com.github.mdr.mash.screen

import com.github.mdr.mash.assist.AssistanceState
import com.github.mdr.mash.os.linux.LinuxFileSystem
import com.github.mdr.mash.repl._
import com.github.mdr.mash.repl.browser._
import com.github.mdr.mash.repl.history.HistorySearchState
import com.github.mdr.mash.screen.Style.StylableString
import com.github.mdr.mash.screen.browser._
import com.github.mdr.mash.utils.{ Dimension, Point, StringUtils }

case class ReplRenderResult(screen: Screen)

case class LinesAndCursorPos(lines: Seq[Line], cursorPos: Point)

/**
  * Render the current state (input buffer, completion state, assistance information etc) into a set of lines of styled
  * characters.
  */
object ReplRenderer {

  private val fileSystem = LinuxFileSystem

  def render(state: ReplState, terminalSize: Dimension): ReplRenderResult =
    state.objectBrowserStateStackOpt match {
      case Some(objectBrowserState) ⇒ renderObjectBrowser(objectBrowserState, terminalSize)
      case None                     ⇒ renderRegularRepl(state, terminalSize)
    }

  private def renderRegularRepl(state: ReplState, terminalSize: Dimension): ReplRenderResult = {
    val bufferScreen = LineBufferRenderer.renderLineBuffer(state, terminalSize)
    val bufferLines = bufferScreen.lines
    val historySearchScreenOpt = state.historySearchStateOpt.map(renderHistorySearchState(_, terminalSize))
    val historySearchLines = historySearchScreenOpt.map(_.lines).getOrElse(Seq())
    val assistanceLines = renderAssistanceState(state.assistanceStateOpt, terminalSize)
    val remainingRows = math.max(0, terminalSize.rows - bufferLines.size - assistanceLines.size - historySearchLines.size)
    val CompletionRenderResult(completionLines) =
      CompletionRenderer.renderCompletions(state.completionStateOpt, terminalSize.copy(rows = remainingRows))
    val lines = bufferLines ++ historySearchLines ++ completionLines ++ assistanceLines
    val truncatedLines = lines.take(terminalSize.rows)
    val newCursorPos = historySearchScreenOpt.map(_.cursorPos.down(bufferLines.size)).getOrElse(bufferScreen.cursorPos)
    val title = fileSystem.pwd.toString
    val screen = Screen(truncatedLines, newCursorPos, cursorVisible = true, title)
    ReplRenderResult(screen)
  }

  private def renderObjectTableBrowser(state: TwoDTableBrowserState, terminalSize: Dimension): ReplRenderResult = {
    val screen = new TwoDTableBrowserRenderer(state, terminalSize).renderObjectBrowser
    ReplRenderResult(screen)
  }

  private def renderSingleObjectBrowser(state: SingleObjectTableBrowserState, terminalSize: Dimension): ReplRenderResult = {
    val screen = new SingleObjectTableBrowserRenderer(state, terminalSize).renderObjectBrowser
    ReplRenderResult(screen)
  }

  private def renderObjectTreeBrowser(state: ObjectTreeBrowserState, terminalSize: Dimension): ReplRenderResult = {
    val screen = new ObjectTreeBrowserRenderer(state, terminalSize).renderObjectBrowser
    ReplRenderResult(screen)
  }

  private def renderValueBrowser(state: ValueBrowserState, terminalSize: Dimension): ReplRenderResult = {
    val screen = new ValueBrowserRenderer(state, terminalSize).renderObjectBrowser
    ReplRenderResult(screen)
  }

  private def renderTextLinesBrowserState(state: TextLinesBrowserState, terminalSize: Dimension): ReplRenderResult = {
    val screen = new TextLinesBrowserRenderer(state, terminalSize).renderObjectBrowser
    ReplRenderResult(screen)
  }

  private def renderObjectBrowser(state: ObjectBrowserStateStack, terminalSize: Dimension): ReplRenderResult =
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
    assistanceStateOpt.map(state ⇒ renderAssistanceState(state, terminalSize)).getOrElse(Seq())

  private def renderAssistanceState(assistanceState: AssistanceState, terminalSize: Dimension): Seq[Line] = {
    val title = assistanceState.title
    val lines = assistanceState.lines
    val boxWidth = math.min(math.max(lines.map(_.size + 4).max, title.size + 6), terminalSize.columns)
    val innerWidth = boxWidth - 4
    val displayTitle = StringUtils.ellipsisise(title, innerWidth)
    val displayLines = lines.map(l ⇒ StringUtils.ellipsisise(l, innerWidth))
    val styledTitle = displayTitle.style(TitleStyle)
    val topLine = Line("┌─ ".style + styledTitle + (" " + "─" * (innerWidth - displayTitle.length - 2) + "─┐").style)

    val bottomLine = Line(("└─" + "─" * innerWidth + "─┘").style)

    val contentLines = displayLines.map(l ⇒ Line(("│ " + l + " " * (innerWidth - l.length) + " │").style))
    topLine +: contentLines :+ bottomLine
  }

  private val TitleStyle = Style(bold = true, foregroundColour = BasicColour.Yellow)

}