package com.github.mdr.mash.repl.browser

import com.github.mdr.mash.lexer.MashLexer._
import com.github.mdr.mash.parser.SafeParens
import com.github.mdr.mash.printer.model.PrintModel
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

  def expressionOpt: Option[String]

  def path: String

  def model: PrintModel

  def rawValue: MashValue

  def withPath(newPath: String): BrowserState

  def getInsertExpression: String

  def setExpression(expression: String): BrowserState

  def acceptExpression: BrowserState

}

case class ObjectBrowserStateStack(browserStates: List[BrowserState]) {
  require(browserStates.nonEmpty)

  def replaceCurrentState(newState: BrowserState): ObjectBrowserStateStack =
    copy(browserStates = newState :: browserStates.tail)

  def pushNewState(newState: BrowserState): ObjectBrowserStateStack =
    copy(browserStates = newState :: browserStates)

  def pop: ObjectBrowserStateStack =
    copy(browserStates = browserStates.tail)

  def headState: BrowserState = browserStates.head

}
