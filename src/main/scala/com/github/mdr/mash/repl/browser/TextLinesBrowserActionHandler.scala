package com.github.mdr.mash.repl.browser

import com.github.mdr.mash.input.InputAction
import com.github.mdr.mash.repl._
import com.github.mdr.mash.repl.browser.ObjectBrowserActions.{ ExpressionInput, Focus, PreviousPage, _ }

trait TextLinesBrowserActionHandler {
  self: ObjectBrowserActionHandler with Repl =>

  protected def handleTextLinesBrowserAction(action: InputAction, browserState: TextLinesBrowserState): Unit = action match {
    case ExitBrowser                     ⇒
      state.objectBrowserStateStackOpt = None
    case Back                            =>
      navigateBack()
    case Open                            =>
      handleOpenItem(browserState)
    case InsertItem                      =>
      handleInsertItem(browserState)
    case InsertWholeItem                 ⇒
      handleInsertWholeItem(browserState)
    case NextItem                        ⇒
      val newState = browserState.adjustSelectedRow(1, terminalRows)
      updateState(newState)
    case PreviousItem                    ⇒
      val newState = browserState.adjustSelectedRow(-1, terminalRows)
      updateState(newState)
    case FirstItem                       ⇒
      val newState = browserState.copy(selectedRow = 0).adjustWindowToFit(terminalRows)
      updateState(newState)
    case LastItem                        ⇒
      val newRow = browserState.model.renderedLines.size - 1
      val newState = browserState.copy(selectedRow = newRow).adjustWindowToFit(terminalRows)
      updateState(newState)
    case NextPage                        ⇒
      val newRow = math.min(browserState.model.renderedLines.size - 1, browserState.selectedRow + terminalRows - 1)
      val newState = browserState.copy(selectedRow = newRow).adjustWindowToFit(terminalRows)
      updateState(newState)
    case PreviousPage                    ⇒
      val newRow = math.max(0, browserState.selectedRow - terminalRows - 1)
      val newState = browserState.copy(selectedRow = newRow).adjustWindowToFit(terminalRows)
      updateState(newState)
    case Focus                           ⇒
      val rawObject = browserState.model.rawValue.items(browserState.selectedRow)
      focus(rawObject, browserState.getInsertExpression, tree = false)
    case ExpressionInput.BeginExpression =>
      updateState(browserState.setExpression(""))
    case _                               =>
  }
}
