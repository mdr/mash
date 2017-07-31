package com.github.mdr.mash.repl.browser.handler

import com.github.mdr.mash.input.InputAction
import com.github.mdr.mash.repl._
import com.github.mdr.mash.repl.browser.HelpBrowserState
import com.github.mdr.mash.repl.browser.ObjectBrowserActions.{ ExpressionInput, Focus, _ }

trait HelpBrowserActionHandler {
  self: ObjectBrowserActionHandler with Repl ⇒

  protected def handleHelpBrowserAction(action: InputAction, browserState: HelpBrowserState): Unit =
    action match {
      case ExitBrowser                     ⇒ state = state.copy(objectBrowserStateStackOpt = None)
      case Back                            ⇒ navigateBack()
      case Open                            ⇒ handleOpenItem(browserState)
      case Copy                            ⇒ handleCopyItem(browserState)
      case InsertItem                      ⇒ handleInsertItem(browserState)
      case InsertWholeItem                 ⇒ handleInsertWholeItem(browserState)
      case NextItem                        ⇒ updateState(browserState.nextLink(terminalRows))
      case PreviousItem                    ⇒ updateState(browserState.previousLink(terminalRows))
      case NextParentItem                  ⇒ selectParentItem(browserState, delta = 1)
      case PreviousParentItem              ⇒ selectParentItem(browserState, delta = -1)
      case Focus                           ⇒ focus(browserState, tree = false)
      case ExpressionInput.BeginExpression ⇒ updateState(browserState.beginExpression)
      case _                               ⇒
    }
}
