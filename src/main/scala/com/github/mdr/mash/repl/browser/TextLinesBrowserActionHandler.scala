package com.github.mdr.mash.repl.browser

import com.github.mdr.mash.input.InputAction
import com.github.mdr.mash.repl._
import com.github.mdr.mash.repl.browser.ObjectBrowserActions.{ ExpressionInput, Focus, PreviousPage, _ }

trait TextLinesBrowserActionHandler {
  self: ObjectBrowserActionHandler with Repl ⇒

  protected def handleTextLinesBrowserAction(action: InputAction, browserState: TextLinesBrowserState): Unit = action match {
    case ExitBrowser                     ⇒
      state.objectBrowserStateStackOpt = None
    case Back                            ⇒
      navigateBack()
    case Open                            ⇒
      handleOpenItem(browserState)
    case InsertItem                      ⇒
      handleInsertItem(browserState)
    case InsertWholeItem                 ⇒
      handleInsertWholeItem(browserState)
    case NextItem                        ⇒
      updateState(browserState.nextItem(terminalRows))
    case PreviousItem                    ⇒
      updateState(browserState.previousItem(terminalRows))
    case FirstItem                       ⇒
      updateState(browserState.firstItem(terminalRows))
    case LastItem                        ⇒
      updateState(browserState.lastItem(terminalRows))
    case NextPage                        ⇒
      updateState(browserState.nextPage(terminalRows))
    case PreviousPage                    ⇒
      updateState(browserState.previousPage(terminalRows))
    case Focus                           ⇒
      focus(browserState, tree = false)
    case ExpressionInput.BeginExpression ⇒
      updateState(browserState.setExpression(""))
    case _                               ⇒
  }
}
