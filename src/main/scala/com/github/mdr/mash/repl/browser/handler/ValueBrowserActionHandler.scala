package com.github.mdr.mash.repl.browser.handler

import com.github.mdr.mash.input.InputAction
import com.github.mdr.mash.repl._
import com.github.mdr.mash.repl.browser.ObjectBrowserActions.{ ExpressionInput, _ }
import com.github.mdr.mash.repl.browser.ValueBrowserState

trait ValueBrowserActionHandler {
  self: ObjectBrowserActionHandler with Repl ⇒

  protected def handleValueBrowserAction(action: InputAction, browserState: ValueBrowserState): Unit =
    action match {
      case ExitBrowser                     ⇒ state = state.copy(objectBrowserStateStackOpt = None)
      case Back                            ⇒ navigateBack()
      case InsertItem                      ⇒ handleInsertItem(browserState)
      case Open                            ⇒ handleOpenItem(browserState)
      case Copy                            ⇒ handleCopyItem(browserState)
      case InsertWholeItem                 ⇒ handleInsertWholeItem(browserState)
      case ExpressionInput.BeginExpression ⇒ updateState(browserState.beginExpression)
      case FocusDirectory                  ⇒ focusDirectory(browserState)
      case ReadFile                        ⇒ readFile(browserState)
      case _                               ⇒
    }

}