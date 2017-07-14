package com.github.mdr.mash.repl.browser

import com.github.mdr.mash.lexer.MashLexer._
import com.github.mdr.mash.parser.ExpressionCombiner.combineSafely
import com.github.mdr.mash.parser.ValueToExpression
import com.github.mdr.mash.printer.model.{ TextLinesModel, _ }
import com.github.mdr.mash.runtime._

object BrowserState {

  def safeProperty(path: String, property: MashValue): String =
    property match {
      case MashString(s, _)                          ⇒
        if (isLegalIdentifier(s))
          combineSafely(path, s".$property")
        else
          combineSafely(path, s"['$property']")
      case _ ⇒
        ValueToExpression(property).map(combineSafely(path, _)).getOrElse(path)
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

  def path: String

  def model: DisplayModel

  def rawValue: MashValue

  def withPath(newPath: String): BrowserState

  def getInsertExpression: String = selectionInfo.path

  def selectionInfo: SelectionInfo

  def beginExpression: BrowserState

  def expressionStateOpt: Option[ExpressionState]

  def setExpression(expression: ExpressionState): BrowserState

  def acceptExpression: BrowserState

}

case class SelectionInfo(path: String, rawObject: MashValue)
