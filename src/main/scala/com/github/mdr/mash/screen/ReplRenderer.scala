package com.github.mdr.mash.screen

import com.github.mdr.mash.assist.AssistanceState
import com.github.mdr.mash.os.linux.LinuxFileSystem
import com.github.mdr.mash.repl._
import com.github.mdr.mash.repl.browser._
import com.github.mdr.mash.repl.history.HistorySearchState
import com.github.mdr.mash.screen.Style.StylableString
import com.github.mdr.mash.screen.browser._
import com.github.mdr.mash.terminal.TerminalInfo
import com.github.mdr.mash.utils.StringUtils

case class ReplRenderResult(screen: Screen, completionColumns: Int = 0)

case class LinesAndCursorPos(lines: Seq[Line], cursorPos: Point)

/**
  * Render the current state (input buffer, completion state, assistance information etc) into a set of lines of styled
  * characters.
  */
object ReplRenderer {

  private val fileSystem = LinuxFileSystem

  def render(state: ReplState, terminalInfo: TerminalInfo): ReplRenderResult = state.objectBrowserStateStackOpt match {
    case Some(objectBrowserState) ⇒ renderObjectBrowser(objectBrowserState, terminalInfo)
    case None                     ⇒ renderRegularRepl(state, terminalInfo)
  }

  private def renderRegularRepl(state: ReplState, terminalInfo: TerminalInfo): ReplRenderResult = {
    val bufferScreen = LineBufferRenderer.renderLineBuffer(state, terminalInfo)
    val bufferLines = bufferScreen.lines
    val historySearchScreenOpt = state.historySearchStateOpt.map(renderHistorySearchState(_, terminalInfo))
    val historySearchLines = historySearchScreenOpt.map(_.lines).getOrElse(Seq())
    val assistanceLines = renderAssistanceState(state.assistanceStateOpt, terminalInfo)
    val remainingRows = math.max(0, terminalInfo.rows - bufferLines.size - assistanceLines.size - historySearchLines.size)
    val CompletionRenderResult(completionLines, numberOfCompletionColumns) =
      CompletionRenderer.renderCompletions(state.completionStateOpt, terminalInfo.copy(rows = remainingRows))
    val lines = bufferLines ++ historySearchLines ++ completionLines ++ assistanceLines
    val truncatedLines = lines.take(terminalInfo.rows)
    val newCursorPos = historySearchScreenOpt.map(_.cursorPos.down(bufferLines.size)).getOrElse(bufferScreen.cursorPos)
    val title = fileSystem.pwd.toString
    val screen = Screen(truncatedLines, newCursorPos, cursorVisible = true, title)
    ReplRenderResult(screen, numberOfCompletionColumns)
  }

  private def renderObjectTableBrowser(state: ObjectsTableBrowserState, terminalInfo: TerminalInfo): ReplRenderResult = {
    val screen = new ObjectsTableBrowserRenderer(state, terminalInfo).renderObjectBrowser
    ReplRenderResult(screen)
  }

  private def renderSingleObjectBrowser(state: SingleObjectTableBrowserState, terminalInfo: TerminalInfo): ReplRenderResult = {
    val screen = new SingleObjectTableBrowserRenderer(state, terminalInfo).renderObjectBrowser
    ReplRenderResult(screen)
  }

  private def renderObjectTreeBrowser(state: ObjectTreeBrowserState, terminalInfo: TerminalInfo): ReplRenderResult = {
    val screen = new ObjectTreeBrowserRenderer(state, terminalInfo).renderObjectBrowser
    ReplRenderResult(screen)
  }

  private def renderValueBrowser(state: ValueBrowserState, terminalInfo: TerminalInfo): ReplRenderResult = {
    val screen = new ValueBrowserRenderer(state, terminalInfo).renderObjectBrowser
    ReplRenderResult(screen)
  }

  private def renderTextLinesBrowserState(state: TextLinesBrowserState, terminalInfo: TerminalInfo): ReplRenderResult = {
    val screen = new TextLinesBrowserRenderer(state, terminalInfo).renderObjectBrowser
    ReplRenderResult(screen)
  }

  private def renderObjectBrowser(state: ObjectBrowserStateStack, terminalInfo: TerminalInfo): ReplRenderResult =
    state.headState match {
      case objectTableBrowserState: ObjectsTableBrowserState       ⇒ renderObjectTableBrowser(objectTableBrowserState, terminalInfo)
      case singleObjectBrowserState: SingleObjectTableBrowserState ⇒ renderSingleObjectBrowser(singleObjectBrowserState, terminalInfo)
      case objectTreeBrowserState: ObjectTreeBrowserState          ⇒ renderObjectTreeBrowser(objectTreeBrowserState, terminalInfo)
      case valueBrowserState: ValueBrowserState                    ⇒ renderValueBrowser(valueBrowserState, terminalInfo)
      case textLinesBrowserState: TextLinesBrowserState            ⇒ renderTextLinesBrowserState(textLinesBrowserState, terminalInfo)
      case _                                                       ⇒ ???
    }

  private def renderHistorySearchState(searchState: HistorySearchState, terminalInfo: TerminalInfo): LinesAndCursorPos = {
    val prefixChars: StyledString = "Incremental history search: ".style
    val searchChars: StyledString = searchState.searchString.style(Style(foregroundColour = BasicColour.Cyan))
    val chars = (prefixChars + searchChars).take(terminalInfo.columns)
    val line = Line((prefixChars + searchChars).take(terminalInfo.columns))
    val cursorPos = Point(0, chars.size)
    LinesAndCursorPos(Seq(line), cursorPos)
  }

  private def renderAssistanceState(assistanceStateOpt: Option[AssistanceState], terminalInfo: TerminalInfo): Seq[Line] =
    assistanceStateOpt.map(state ⇒ renderAssistanceState(state, terminalInfo)).getOrElse(Seq())

  private def renderAssistanceState(assistanceState: AssistanceState, terminalInfo: TerminalInfo): Seq[Line] = {
    val title = assistanceState.title
    val lines = assistanceState.lines
    val boxWidth = math.min(math.max(lines.map(_.size + 4).max, title.size + 6), terminalInfo.columns)
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