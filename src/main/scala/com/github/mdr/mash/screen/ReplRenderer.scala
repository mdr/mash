package com.github.mdr.mash.screen

import com.github.mdr.mash.assist.AssistanceState
import com.github.mdr.mash.os.linux.LinuxFileSystem
import com.github.mdr.mash.repl._
import com.github.mdr.mash.repl.browser._
import com.github.mdr.mash.runtime.MashObject
import com.github.mdr.mash.screen.browser._
import com.github.mdr.mash.utils.{ Dimensions, Point }

case class LinesAndCursorPos(lines: Seq[Line], cursorPos: Point)

/**
  * Render the current state (input buffer, completion state, assistance information etc) into a set of lines of styled
  * characters.
  */
object ReplRenderer {

  private val fileSystem = LinuxFileSystem

  def render(state: ReplState, terminalSize: Dimensions, globalVariables: MashObject, bareWords: Boolean): Screen =
    state.objectBrowserStateStackOpt match {
      case Some(objectBrowserState) ⇒ renderObjectBrowser(objectBrowserState, terminalSize)
      case None                     ⇒ renderRegularRepl(state, terminalSize, globalVariables, bareWords)
    }

  private def renderRegularRepl(state: ReplState, terminalSize: Dimensions, globalVariables: MashObject, bareWords: Boolean): Screen = {
    val LinesAndCursorPos(bufferLines, bufferCursorPos) = LineBufferRenderer.renderLineBuffer(state, terminalSize, globalVariables, bareWords)
    val historySearchScreenOpt = state.historySearchStateOpt.map(IncrementalHistorySearchRenderer.renderHistorySearchState(_, terminalSize))
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

  private def renderObjectTableBrowser(state: TwoDTableBrowserState, terminalSize: Dimensions): Screen =
    new TwoDTableBrowserRenderer(state, terminalSize).renderObjectBrowser

  private def renderSingleObjectBrowser(state: SingleObjectTableBrowserState, terminalSize: Dimensions): Screen =
    new SingleObjectTableBrowserRenderer(state, terminalSize).renderObjectBrowser

  private def renderObjectTreeBrowser(state: ObjectTreeBrowserState, terminalSize: Dimensions): Screen =
    new ObjectTreeBrowserRenderer(state, terminalSize).renderObjectBrowser

  private def renderValueBrowser(state: ValueBrowserState, terminalSize: Dimensions): Screen =
    new ValueBrowserRenderer(state, terminalSize).renderObjectBrowser

  private def renderTextLinesBrowserState(state: TextLinesBrowserState, terminalSize: Dimensions): Screen =
    new TextLinesBrowserRenderer(state, terminalSize).renderObjectBrowser

  private def renderObjectBrowser(state: ObjectBrowserStateStack, terminalSize: Dimensions): Screen =
    state.headState match {
      case browserState: TwoDTableBrowserState         ⇒ renderObjectTableBrowser(browserState, terminalSize)
      case browserState: SingleObjectTableBrowserState ⇒ renderSingleObjectBrowser(browserState, terminalSize)
      case browserState: ObjectTreeBrowserState        ⇒ renderObjectTreeBrowser(browserState, terminalSize)
      case browserState: ValueBrowserState             ⇒ renderValueBrowser(browserState, terminalSize)
      case browserState: TextLinesBrowserState         ⇒ renderTextLinesBrowserState(browserState, terminalSize)
      case browserState                                ⇒ throw new RuntimeException(s"Unknown browser state of type: ${state.getClass.getSimpleName}")
    }

  private def renderAssistanceState(assistanceStateOpt: Option[AssistanceState], terminalSize: Dimensions): Seq[Line] =
    assistanceStateOpt.toSeq.flatMap(AssistanceRenderer.render(_, terminalSize))

}