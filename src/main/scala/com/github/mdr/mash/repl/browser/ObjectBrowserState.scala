package com.github.mdr.mash.repl.browser

import com.github.mdr.mash.lexer.MashLexer._
import com.github.mdr.mash.parser.SafeParens
import com.github.mdr.mash.runtime.MashValue

object BrowserState {

  def safeProperty(path: String, property: String): String = {
    val safePath = SafeParens.safeParens(path)
    if (isLegalIdentifier(property))
      s"$safePath.$property"
    else
      s"$safePath['$property']"
  }

}

trait BrowserState {

  def rawValue: MashValue

  def path: String

  def withPath(newPath: String): BrowserState

  def getInsertExpression: String
}

case class ObjectBrowserState(browserStates: List[BrowserState]) {
  require(browserStates.nonEmpty)

  def replaceCurrentState(newState: BrowserState): ObjectBrowserState =
    copy(browserStates = newState :: browserStates.tail)

  def pushNewState(newState: BrowserState): ObjectBrowserState =
    copy(browserStates = newState :: browserStates)

  def pop: ObjectBrowserState =
    copy(browserStates = browserStates.tail)

  def browserState: BrowserState = browserStates.head

}
