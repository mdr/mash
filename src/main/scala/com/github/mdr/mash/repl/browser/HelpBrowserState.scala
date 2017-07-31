package com.github.mdr.mash.repl.browser

import com.github.mdr.mash.printer.model.{ HelpModel, Link }
import com.github.mdr.mash.runtime.MashValue
import com.github.mdr.mash.utils.Utils._

case class HelpBrowserState(model: HelpModel,
                            currentLinkIndexOpt: Option[Int] = None,
                            firstRow: Int = 0,
                            path: String,
                            expressionStateOpt: Option[ExpressionState] = None) extends BrowserState {

  def nextLink(terminalRows: Int): HelpBrowserState = adjustCurrentLink(delta = 1, terminalRows)

  def previousLink(terminalRows: Int): HelpBrowserState = adjustCurrentLink(delta = -1, terminalRows)

  private def adjustCurrentLink(delta: Int, terminalRows: Int): HelpBrowserState = {
    val newLinkIndexOpt =
      currentLinkIndexOpt
        .map(currentLinkIndex ⇒ (currentLinkIndex + model.numberOfLinks + delta) % model.numberOfLinks)
        .orElse((model.numberOfLinks > 0).option(0))
    copy(currentLinkIndexOpt = newLinkIndexOpt).adjustWindowToFit(terminalRows)
  }

  override def rawValue: MashValue = model.rawValue

  override def withPath(newPath: String): BrowserState = copy(path = newPath)

  override def withExpressionState(expressionStateOpt: Option[ExpressionState]): BrowserState =
    copy(expressionStateOpt = expressionStateOpt)

  override def selectionInfo: SelectionInfo = currentLinkOpt match {
    case Some(link) ⇒ SelectionInfo(path, link.target) // Path TODO
    case None       ⇒ SelectionInfo(path, rawValue)
  }

  def currentLinkOpt: Option[Link] = currentLinkIndexOpt.map(model.links)

  def windowSize(terminalRows: Int) = terminalRows - 4 // 2 status rows and two border rows

  def adjustWindowToFit(terminalRows: Int): HelpBrowserState = currentLinkOpt.map(_.line) match {
    case Some(currentRow) ⇒
      var newState = this
      val delta = currentRow - (firstRow + windowSize(terminalRows) - 1)
      if (delta >= 0)
        newState = newState.adjustFirstRow(delta)

      val delta2 = firstRow - currentRow
      if (delta2 >= 0)
        newState = newState.adjustFirstRow(-delta2)

      newState
    case None             ⇒ this
  }

  def adjustFirstRow(delta: Int): HelpBrowserState = copy(firstRow = firstRow + delta)
}
