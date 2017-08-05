package com.github.mdr.mash.repl.browser

import com.github.mdr.mash.parser.ExpressionCombiner._
import com.github.mdr.mash.printer.model.{ HelpModel, Link, LinkPath }
import com.github.mdr.mash.runtime.MashValue
import com.github.mdr.mash.utils.Utils._

case class HelpBrowserState(model: HelpModel,
                            currentRow: Int = 0,
                            firstRow: Int = 0,
                            path: String,
                            expressionStateOpt: Option[ExpressionState] = None) extends BrowserState {

  def previousItem(terminalRows: Int): HelpBrowserState = adjustSelectedRow(-1, terminalRows)

  def nextItem(terminalRows: Int): HelpBrowserState = adjustSelectedRow(1, terminalRows)

  def adjustSelectedRow(delta: Int, terminalRows: Int): HelpBrowserState = {
    val newRow = 0 max currentRow + delta min numberOfRows - 1
    this.when(numberOfRows > 0, _.copy(currentRow = newRow).adjustWindowToFit(terminalRows))
  }

  def numberOfRows = model.numberOfRows

  override def rawValue: MashValue = model.rawValue

  override def withPath(newPath: String): BrowserState = copy(path = newPath)

  override def withExpressionState(expressionStateOpt: Option[ExpressionState]): BrowserState =
    copy(expressionStateOpt = expressionStateOpt)

  override def selectionInfoOpt: Option[SelectionInfo] = currentLinkOpt map { link ⇒
    val newPath = link.linkPath match {
      case LinkPath.Absolute(absolutePath) ⇒ absolutePath
      case LinkPath.Relative(pathFragment) ⇒ combineSafely(path, pathFragment)
    }
    SelectionInfo(newPath, link.target)
  }

  def currentLinkOpt: Option[Link] = model.links.find(_.line == currentRow)

  def windowSize(terminalRows: Int) = terminalRows - 4 // 2 status rows and two border rows

  def adjustWindowToFit(terminalRows: Int): HelpBrowserState = {
    var newState = this

    val delta = currentRow - (firstRow + windowSize(terminalRows) - 1)
    if (delta >= 0)
      newState = newState.adjustFirstRow(delta)

    val delta2 = firstRow - currentRow
    if (delta2 >= 0)
      newState = newState.adjustFirstRow(-delta2)

    newState
  }

  def adjustFirstRow(delta: Int): HelpBrowserState = copy(firstRow = firstRow + delta)

  def firstItem(terminalRows: Int): HelpBrowserState =
    copy(currentRow = 0).adjustWindowToFit(terminalRows)

  def lastItem(terminalRows: Int): HelpBrowserState =
    copy(currentRow = numberOfRows - 1).adjustWindowToFit(terminalRows)

  def nextPage(terminalRows: Int): HelpBrowserState = {
    val newRow = currentRow + windowSize(terminalRows) - 1 min model.numberOfRows - 1
    copy(currentRow = newRow).adjustWindowToFit(terminalRows)
  }

  def previousPage(terminalRows: Int): HelpBrowserState = {
    val newRow = 0 max currentRow - windowSize(terminalRows) - 1
    copy(currentRow = newRow).adjustWindowToFit(terminalRows)
  }

}
