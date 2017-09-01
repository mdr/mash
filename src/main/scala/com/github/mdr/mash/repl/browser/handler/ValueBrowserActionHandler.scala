package com.github.mdr.mash.repl.browser.handler

import com.github.mdr.mash.input.InputAction
import com.github.mdr.mash.repl._
import com.github.mdr.mash.repl.browser.ObjectBrowserActions.{ ExpressionInput, _ }
import com.github.mdr.mash.repl.browser.ValueBrowserState

trait ValueBrowserActionHandler {
  self: ObjectBrowserActionHandler with Repl ⇒

  protected def handleValueBrowserAction(action: InputAction, browserState: ValueBrowserState): Unit =
    commonBrowserActionHandler(browserState)
      .lift(action)

}