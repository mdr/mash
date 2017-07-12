package com.github.mdr.mash.repl.browser

import com.github.mdr.mash.evaluator.ToStringifier
import com.github.mdr.mash.lexer.MashLexer._
import com.github.mdr.mash.parser.ExpressionCombiner.combineSafely
import com.github.mdr.mash.parser.StringEscapes
import com.github.mdr.mash.printer.model.{ TextLinesModel, _ }
import com.github.mdr.mash.runtime._

import scala.PartialFunction.condOpt

object BrowserState {

  def safeProperty(path: String, property: MashValue): String =
    property match {
      case MashString(s, _)                          ⇒
        if (isLegalIdentifier(s))
          combineSafely(path, s".$property")
        else
          combineSafely(path, s"['$property']")
      case _ ⇒
        expressionFor(property).map(combineSafely(path, _)).getOrElse(path)
    }

  def expressionFor(value: MashValue): Option[String] = condOpt(value) {
    case MashNull | _: MashBoolean | _: MashNumber ⇒ ToStringifier.stringify(value)
    case s: MashString                             ⇒ s"'${StringEscapes.escapeChars(s.s)}'"
  }

  def fromModel(displayModel: DisplayModel, path: String): BrowserState =
    displayModel match {
      case model: TwoDTableModel         ⇒ TwoDTableBrowserState(model, path = path)
      case model: SingleObjectTableModel ⇒ SingleObjectTableBrowserState(model, path = path)
      case model: ObjectTreeModel        ⇒ ObjectTreeBrowserState.initial(model, path = path)
      case model: ValueModel             ⇒ new ValueBrowserState(model, path = path)
      case model: TextLinesModel         ⇒ new TextLinesBrowserState(model, path = path)
      case _                             ⇒ throw new RuntimeException("Unknown type of print model: " + displayModel)
    }

}

trait BrowserState {

  def expressionOpt: Option[String]

  def path: String

  def model: DisplayModel

  def rawValue: MashValue

  def withPath(newPath: String): BrowserState

  def getInsertExpression: String = selectionInfo.path

  def selectionInfo: SelectionInfo

  def setExpression(expression: String): BrowserState

  def acceptExpression: BrowserState

}

case class SelectionInfo(path: String, rawObject: MashValue)
