package com.github.mdr.mash.view.render.browser

import java.nio.file.Files

import com.github.mdr.mash.view.render.{ KeyHint, LinesAndCursorPos, MashRenderingContext }
import com.github.mdr.mash.repl.browser.TwoDTableBrowserState
import com.github.mdr.mash.repl.browser.handler.ObjectBrowserActionHandler
import com.github.mdr.mash.screen.Style.StylableString
import com.github.mdr.mash.screen._
import com.github.mdr.mash.utils.Dimensions
import com.github.mdr.mash.utils.Utils._
import com.github.mdr.mash.view.common.TwoDTableCommonRenderer
import com.github.mdr.mash.view.common.TwoDTableCommonRenderer.ExtraRenderingInfo

class TwoDTableBrowserRenderer(state: TwoDTableBrowserState,
                               terminalSize: Dimensions,
                               mashRenderingContext: MashRenderingContext)
  extends AbstractBrowserRenderer(state, terminalSize, mashRenderingContext) {

  protected def renderLines: LinesAndCursorPos =
    combineUpperStatusLines(renderUpperStatusLines, renderTableLines :+ renderStatusLine)

  private def renderTableLines: Seq[Line] = {
    val currentRowIndexOpt = Some(state.currentRow).filterNot(_ ⇒ state.expressionStateOpt.isDefined)
    val currentColumnIndexOpt = state.currentColumnOpt.filterNot(_ ⇒ state.expressionStateOpt.isDefined)
    val extraRenderingInfo = ExtraRenderingInfo(Some(state.markedRows), currentRowIndexOpt, currentColumnIndexOpt, state.searchStateOpt)
    val commonRenderer = new TwoDTableCommonRenderer(state.model, terminalSize, extraRenderingInfo)
    commonRenderer.renderTableLines(state.firstRow, windowSize)
  }

  private def renderRegularStatusLine: Line = {
    import KeyHint._
    val selectedPathOpt = ObjectBrowserActionHandler.getSelectedPath(state)
    val isDir = selectedPathOpt.exists(Files.isDirectory(_))
    val isFile = selectedPathOpt.exists(Files.isRegularFile(_))
    val hints = Seq(Exit, Mark, Focus, Back, Insert, InsertWhole, Tree, Search, Expression, Open, Copy) ++
      isDir.option(Dir) ++
      isFile.option(Read) ++
      Seq(NextParentResult, PreviousParentResult) ++
      state.currentColumnOpt.toSeq.flatMap(_ ⇒ Seq(Row, HideColumn))
    val countChars = s"${currentRow + 1}/${model.rows.size}".style(Style(inverse = true))
    Line(countChars + " (".style + renderKeyHints(hints) + ")".style)
  }

  private def renderStatusLine: Line =
    state.searchStateOpt match {
      case Some(searchState) ⇒ StatusLineRenderers.renderIncrementalSearchStatusLine(currentRow, searchState)
      case None              ⇒
        state.expressionStateOpt match {
          case Some(expressionState) ⇒ StatusLineRenderers.renderExpressionInputStatusLine
          case None                  ⇒ renderRegularStatusLine
        }
    }

  private def model = state.model

  private def currentRow = state.currentRow

  protected val windowSize = state.windowSize(terminalSize.rows)

}