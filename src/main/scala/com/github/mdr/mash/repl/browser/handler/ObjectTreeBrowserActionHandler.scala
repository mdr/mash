package com.github.mdr.mash.repl.browser.handler

import com.github.mdr.mash.input.InputAction
import com.github.mdr.mash.repl._
import com.github.mdr.mash.repl.browser.ObjectBrowserActions._
import com.github.mdr.mash.repl.browser.ObjectTreeBrowserState

trait ObjectTreeBrowserActionHandler {
  self: ObjectBrowserActionHandler with Repl ⇒

  protected def handleObjectTreeBrowserAction(action: InputAction, browserState: ObjectTreeBrowserState): Unit =
    commonBrowserActionHandler(browserState)
      .orElse(objectTreeBrowserActionHandler(browserState))
      .lift(action)

  private def objectTreeBrowserActionHandler(browserState: ObjectTreeBrowserState): PartialFunction[InputAction, Unit] = {
    case NextColumn     ⇒ updateState(browserState.right)
    case PreviousColumn ⇒ updateState(browserState.left)
    case NextItem       ⇒ updateState(browserState.nextItem(terminalRows))
    case PreviousItem   ⇒ updateState(browserState.previousItem(terminalRows))
    case ViewAsTree     ⇒ updateState(getNewBrowserState(browserState.rawValue, browserState.path))
  }
  
}
