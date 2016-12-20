package com.github.mdr.mash.repl.browser

import com.github.mdr.mash.input.InputAction
import com.github.mdr.mash.repl._
import com.github.mdr.mash.repl.browser.ObjectBrowserActions.{ ExpressionInput, Focus, _ }

trait SingleObjectBrowserActionHandler {
  self: ObjectBrowserActionHandler with Repl =>

  protected def handleSingleObjectBrowserAction(action: InputAction, browserState: SingleObjectTableBrowserState): Unit =
    action match {
      case NextItem                        ⇒
        updateState(browserState.nextItem(terminalRows))
      case PreviousItem                    ⇒
        val newState = browserState.previousItem(terminalRows)
        updateState(newState)
      case FirstItem                       ⇒
        updateState(browserState.firstItem(terminalRows))
      case LastItem                        ⇒
        updateState(browserState.lastItem(terminalRows))
      case ExitBrowser                     ⇒
        state.objectBrowserStateStackOpt = None
      case Focus                           ⇒
        val field = browserState.selectedField
        val value = browserState.selectedRawValue
        val newPath = BrowserState.safeProperty(browserState.path, field)
        focus(value, newPath, tree = false)
      case Back                            =>
        navigateBack()
      case InsertItem                      ⇒
        handleInsertItem(browserState)
      case InsertWholeItem                 ⇒
        handleInsertWholeItem(browserState)
      case Open                            =>
        handleOpenItem(browserState)
      case ViewAsTree                      =>
        viewAsTree(browserState)
      case ExpressionInput.BeginExpression =>
        updateState(browserState.setExpression(""))
      case _                               =>

    }
}
