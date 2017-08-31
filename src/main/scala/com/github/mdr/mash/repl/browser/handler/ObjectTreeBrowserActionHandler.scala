package com.github.mdr.mash.repl.browser.handler

import com.github.mdr.mash.input.InputAction
import com.github.mdr.mash.repl._
import com.github.mdr.mash.repl.browser.ObjectBrowserActions.{ ExpressionInput, Focus, _ }
import com.github.mdr.mash.repl.browser.ObjectTreeBrowserState

trait ObjectTreeBrowserActionHandler {
  self: ObjectBrowserActionHandler with Repl ⇒

  protected def handleObjectTreeBrowserAction(action: InputAction, browserState: ObjectTreeBrowserState): Unit =
    action match {
      case Focus                           ⇒ focus(browserState, tree = true)
      case FocusDirectory                  ⇒ focusDirectory(browserState)
      case ReadFile                        ⇒ readFile(browserState)
      case ExitBrowser                     ⇒ state = state.copy(objectBrowserStateStackOpt = None)
      case Back                            ⇒ navigateBack()
      case NextColumn                      ⇒ updateState(browserState.right)
      case PreviousColumn                  ⇒ updateState(browserState.left)
      case NextItem                        ⇒ updateState(browserState.nextItem(terminalRows))
      case PreviousItem                    ⇒ updateState(browserState.previousItem(terminalRows))
      case NextParentItem                  ⇒ selectParentItem(browserState, delta = 1, tree = true)
      case PreviousParentItem              ⇒ selectParentItem(browserState, delta = -1, tree = true)
      case ViewAsTree                      ⇒ updateState(getNewBrowserState(browserState.rawValue, browserState.path))
      case InsertItem                      ⇒ handleInsertItem(browserState)
      case Open                            ⇒ handleOpenItem(browserState)
      case Copy                            ⇒ handleCopyItem(browserState)
      case InsertWholeItem                 ⇒ handleInsertWholeItem(browserState)
      case ExpressionInput.BeginExpression ⇒ updateState(browserState.beginExpression)
      case _                               ⇒
    }
}
