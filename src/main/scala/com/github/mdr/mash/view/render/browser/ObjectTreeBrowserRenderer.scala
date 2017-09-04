package com.github.mdr.mash.view.render.browser

import com.github.mdr.mash.view.render.{ KeyHint, LinesAndCursorPos, MashRenderingContext }
import com.github.mdr.mash.repl.browser.ObjectTreeBrowserState
import com.github.mdr.mash.screen._
import com.github.mdr.mash.utils.Dimensions
import com.github.mdr.mash.utils.Utils._
import com.github.mdr.mash.view.common.ObjectTreeCommonRenderer

class ObjectTreeBrowserRenderer(state: ObjectTreeBrowserState,
                                terminalSize: Dimensions,
                                mashRenderingContext: MashRenderingContext)
  extends AbstractBrowserRenderer(state, terminalSize, mashRenderingContext) {

  protected def renderLines: LinesAndCursorPos = {
    val renderer = new ObjectTreeCommonRenderer(state.model, Some(state.selectionPath), terminalSize)
    val treeLines = renderer.renderTableLines.window(state.firstRow, windowSize)
    combineUpperStatusLines(renderUpperStatusLines, treeLines :+ renderStatusLine)
  }

  private def renderRegularStatusLine: Line = {
    import KeyHint._
    val hints = Seq(Exit, Back, Focus, Insert, InsertWhole, Table, Open, Copy, Read, NextParentResult, PreviousParentResult)
    Line(renderKeyHints(hints))
  }

  private def renderStatusLine: Line =
    state.expressionStateOpt match {
      case Some(_) ⇒ StatusLineRenderers.renderExpressionInputStatusLine
      case None    ⇒ renderRegularStatusLine
    }

  protected val windowSize = terminalSize.rows - 2 // two status lines

}
