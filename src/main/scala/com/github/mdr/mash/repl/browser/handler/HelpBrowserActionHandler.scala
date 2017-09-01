package com.github.mdr.mash.repl.browser.handler

import com.github.mdr.mash.input.InputAction
import com.github.mdr.mash.repl._
import com.github.mdr.mash.repl.browser.HelpBrowserState
import com.github.mdr.mash.repl.browser.ObjectBrowserActions._

trait HelpBrowserActionHandler {
  self: ObjectBrowserActionHandler with Repl ⇒

  protected def handleHelpBrowserAction(action: InputAction, browserState: HelpBrowserState): Unit =
    commonBrowserActionHandler(browserState)
      .orElse(helpBrowserActionHandler(browserState))
      .lift(action)

  private def helpBrowserActionHandler(browserState: HelpBrowserState): PartialFunction[InputAction, Unit] = {
    case NextItem     ⇒ updateState(browserState.nextItem(terminalRows))
    case PreviousItem ⇒ updateState(browserState.previousItem(terminalRows))
    case NextPage     ⇒ updateState(browserState.nextPage(terminalRows))
    case PreviousPage ⇒ updateState(browserState.previousPage(terminalRows))
    case FirstItem    ⇒ updateState(browserState.firstItem(terminalRows))
    case LastItem     ⇒ updateState(browserState.lastItem(terminalRows))
  }
}
