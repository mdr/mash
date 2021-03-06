package com.github.mdr.mash.view.render.browser

import com.github.mdr.mash.view.render.{ KeyHint, LinesAndCursorPos, MashRenderingContext }
import com.github.mdr.mash.repl.browser.TextLinesBrowserState
import com.github.mdr.mash.screen.Style.StylableString
import com.github.mdr.mash.screen._
import com.github.mdr.mash.utils.Dimensions
import com.github.mdr.mash.utils.Utils._

class TextLinesBrowserRenderer(state: TextLinesBrowserState,
                               terminalSize: Dimensions,
                               mashRenderingContext: MashRenderingContext)
  extends AbstractBrowserRenderer(state, terminalSize, mashRenderingContext) {

  protected def renderDataLines: Seq[Line] =
    for {
      (lineContents, index) ← state.model.renderedLines.zipWithIndex.window(state.firstRow, windowSize)
      isSelected = index == state.selectedRow && state.expressionStateOpt.isEmpty
      actualLineContents = if (isSelected && lineContents.isEmpty) " " else lineContents
    } yield Line(actualLineContents.style(Style(inverse = isSelected)))

  protected def renderLines: LinesAndCursorPos =
    combineUpperStatusLines(renderUpperStatusLines, renderDataLines ++ Seq(renderStatusLine))

  private def renderRegularStatusLine = {
    import KeyHint._
    val hints = Seq(Exit, Back, InsertWhole, NextParentResult, PreviousParentResult)
    val countChars = renderCount(state.selectedRow + 1, state.model.renderedLines.size)
    Line(countChars + " (".style + renderKeyHints(hints) + ")".style)
  }

  private def renderStatusLine: Line =
    state.expressionStateOpt match {
      case Some(_) ⇒ StatusLineRenderers.renderExpressionInputStatusLine
      case None    ⇒ renderRegularStatusLine
    }

  protected val windowSize = state.windowSize(terminalSize.rows)

}
