package com.github.mdr.mash.repl.browser.handler

import com.github.mdr.mash.input.InputAction
import com.github.mdr.mash.repl._
import com.github.mdr.mash.repl.browser.ObjectBrowserActions.{ PreviousPage, _ }
import com.github.mdr.mash.repl.browser.TextLinesBrowserState

trait TextLinesBrowserActionHandler {
  self: ObjectBrowserActionHandler with Repl ⇒

  protected def handleTextLinesBrowserAction(action: InputAction, browserState: TextLinesBrowserState): Unit =
    commonBrowserActionHandler(browserState)
      .orElse(textLinesBrowserActionHandler(browserState))
      .lift(action)

  private def textLinesBrowserActionHandler(browserState: TextLinesBrowserState): PartialFunction[InputAction, Unit] = {
    case NextItem                        ⇒ updateState(browserState.nextItem(terminalRows))
    case PreviousItem                    ⇒ updateState(browserState.previousItem(terminalRows))
    case FirstItem                       ⇒ updateState(browserState.firstItem(terminalRows))
    case LastItem                        ⇒ updateState(browserState.lastItem(terminalRows))
    case NextPage                        ⇒ updateState(browserState.nextPage(terminalRows))
    case PreviousPage                    ⇒ updateState(browserState.previousPage(terminalRows))
  }
}
