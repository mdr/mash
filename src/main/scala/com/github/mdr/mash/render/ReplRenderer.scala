package com.github.mdr.mash.render

import com.github.mdr.mash.assist.AssistanceState
import com.github.mdr.mash.evaluator.TildeExpander
import com.github.mdr.mash.os.{ EnvironmentInteractions, FileSystem }
import com.github.mdr.mash.render.browser._
import com.github.mdr.mash.repl._
import com.github.mdr.mash.repl.browser._
import com.github.mdr.mash.runtime.MashObject
import com.github.mdr.mash.screen.{ Line, Screen }
import com.github.mdr.mash.utils.{ Dimensions, Point }
import com.github.mdr.mash.utils.Utils._

case class LinesAndCursorPos(lines: Seq[Line], cursorPosOpt: Option[Point] = None)

/**
  * Render the current state (input buffer, completion state, assistance information etc) into a set of lines of styled
  * characters.
  */
class ReplRenderer(fileSystem: FileSystem,
                   envInteractions: EnvironmentInteractions,
                   terminalSize: Dimensions,
                   globalVariables: MashObject,
                   bareWords: Boolean,
                   discoBorders: Boolean) {

  def render(state: ReplState): Screen = {
    val fullScreen = state.objectBrowserStateStackOpt match {
      case Some(objectBrowserState) ⇒ renderObjectBrowser(objectBrowserState, getMashRenderingContext(state))
      case None                     ⇒ renderRegularRepl(state)
    }
    fullScreen.truncate(terminalSize).when(discoBorders, DiscoBorders.addDiscoBorders)
  }

  private def renderRegularRepl(state: ReplState): Screen = {
    val mashRenderingContext = getMashRenderingContext(state)
    val LinesAndCursorPos(bufferLines, bufferCursorPosOpt) =
      new LineBufferRenderer(envInteractions, fileSystem).renderLineBuffer(state, terminalSize, mashRenderingContext)

    val historySearchLinesAndCursorPosOpt = state.incrementalHistorySearchStateOpt.map(
      IncrementalHistorySearchRenderer.renderHistorySearchState(_, terminalSize))
    val historySearchLines = historySearchLinesAndCursorPosOpt.map(_.lines).getOrElse(Seq())

    val newCursorPosOpt: Option[Point] = 
      historySearchLinesAndCursorPosOpt
        .flatMap(_.cursorPosOpt)
        .map(_.down(bufferLines.size))
        .orElse(bufferCursorPosOpt)
    
    val assistanceLines = renderAssistanceState(state.assistanceStateOpt, terminalSize)

    val remainingRows = 0 max terminalSize.rows - bufferLines.size - assistanceLines.size - historySearchLines.size
    val remainingSpace = terminalSize.copy(rows = remainingRows)
    val CompletionRenderResult(completionLines) =
      CompletionRenderer.renderCompletions(state.completionStateOpt, remainingSpace)

    val lines = bufferLines ++ historySearchLines ++ assistanceLines ++ completionLines

    val title = new TildeExpander(envInteractions).retilde(fileSystem.pwd.toString)
    Screen(lines, newCursorPosOpt, title)
  }

  private def getMashRenderingContext(state: ReplState): MashRenderingContext =
    MashRenderingContext(Some(globalVariables), bareWords, state.mish)

  private def renderObjectTableBrowser(state: TwoDTableBrowserState, mashRenderingContext: MashRenderingContext): Screen =
    new TwoDTableBrowserRenderer(state, terminalSize, mashRenderingContext).renderObjectBrowser

  private def renderSingleObjectBrowser(state: SingleObjectTableBrowserState, mashRenderingContext: MashRenderingContext): Screen =
    new SingleObjectTableBrowserRenderer(state, terminalSize, mashRenderingContext).renderObjectBrowser

  private def renderObjectTreeBrowser(state: ObjectTreeBrowserState, mashRenderingContext: MashRenderingContext): Screen =
    new ObjectTreeBrowserRenderer(state, terminalSize, mashRenderingContext).renderObjectBrowser

  private def renderValueBrowser(state: ValueBrowserState, mashRenderingContext: MashRenderingContext): Screen =
    new ValueBrowserRenderer(state, terminalSize, mashRenderingContext).renderObjectBrowser

  private def renderTextLinesBrowserState(state: TextLinesBrowserState, mashRenderingContext: MashRenderingContext): Screen =
    new TextLinesBrowserRenderer(state, terminalSize, mashRenderingContext).renderObjectBrowser

  private def renderHelpBrowserState(state: HelpBrowserState, mashRenderingContext: MashRenderingContext): Screen =
    new HelpBrowserRenderer(state, terminalSize, mashRenderingContext).renderObjectBrowser

  private def renderObjectBrowser(state: ObjectBrowserStateStack, mashRenderingContext: MashRenderingContext): Screen =
    state.headState match {
      case browserState: TwoDTableBrowserState         ⇒ renderObjectTableBrowser(browserState, mashRenderingContext)
      case browserState: SingleObjectTableBrowserState ⇒ renderSingleObjectBrowser(browserState, mashRenderingContext)
      case browserState: ObjectTreeBrowserState        ⇒ renderObjectTreeBrowser(browserState, mashRenderingContext)
      case browserState: ValueBrowserState             ⇒ renderValueBrowser(browserState, mashRenderingContext)
      case browserState: TextLinesBrowserState         ⇒ renderTextLinesBrowserState(browserState, mashRenderingContext)
      case browserState: HelpBrowserState              ⇒ renderHelpBrowserState(browserState, mashRenderingContext)
      case _                                           ⇒ throw new RuntimeException(s"Unknown browser state of type: ${state.getClass.getSimpleName}")
    }

  private def renderAssistanceState(assistanceStateOpt: Option[AssistanceState], terminalSize: Dimensions): Seq[Line] =
    assistanceStateOpt.toSeq.flatMap(AssistanceRenderer.render(_, terminalSize))

}