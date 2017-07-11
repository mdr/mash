package com.github.mdr.mash.repl.browser

import com.github.mdr.mash.evaluator.ToStringifier
import com.github.mdr.mash.lexer.MashLexer._
import com.github.mdr.mash.parser.ExpressionCombiner.combineSafely
import com.github.mdr.mash.printer.model.PrintModel
import com.github.mdr.mash.runtime._

object BrowserState {

  def safeProperty(path: String, property: MashValue): String =
    property match {
      case MashString(s, _)                          ⇒
        if (isLegalIdentifier(s))
          combineSafely(path, s".$property")
        else
          combineSafely(path, s"['$property']")
      case MashNull | _: MashBoolean | _: MashNumber ⇒
        val propertyName = ToStringifier.stringify(property)
        combineSafely(path, s"['$propertyName']")
      case _                                         ⇒
        path
    }

}

trait BrowserState {

  def expressionOpt: Option[String]

  def path: String

  def model: PrintModel

  def rawValue: MashValue

  def withPath(newPath: String): BrowserState

  def getInsertExpression: String = selectionInfo.path

  def selectionInfo: SelectionInfo

  def setExpression(expression: String): BrowserState

  def acceptExpression: BrowserState

}

case class SelectionInfo(path: String, rawObject: MashValue)