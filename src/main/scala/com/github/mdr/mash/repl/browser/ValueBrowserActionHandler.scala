package com.github.mdr.mash.repl.browser

import com.github.mdr.mash.input.InputAction
import com.github.mdr.mash.repl._
import com.github.mdr.mash.repl.browser.ObjectBrowserActions.{ ExpressionInput, _ }

trait ValueBrowserActionHandler {
  self: ObjectBrowserActionHandler with Repl ⇒

  protected def handleValueBrowserAction(action: InputAction, browserState: ValueBrowserState): Unit =
    action match {
      case ExitBrowser                     ⇒ state.objectBrowserStateStackOpt = None
      case Back                            ⇒ navigateBack()
      case InsertItem                      ⇒ handleInsertItem(browserState)
      case Open                            ⇒ handleOpenItem(browserState)
      case Copy                            ⇒ handleCopyItem(browserState)
      case InsertWholeItem                 ⇒ handleInsertWholeItem(browserState)
      case ExpressionInput.BeginExpression ⇒ updateState(browserState.beginExpression)
      case _                               ⇒
    }

}