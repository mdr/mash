package com.github.mdr.mash.completions

/**
 * @displayText -- text to display in the list of completions options
 * @insertTextOpt -- text to insert if this replacement is selected (else default to the displayText)
 */
case class Completion(
    displayText: String,
    insertTextOpt: Option[String] = None,
    isQuoted: Boolean = false,
    completionTypeOpt: Option[CompletionType] = None,
    descriptionOpt: Option[String] = None) {

  def insertText = insertTextOpt.getOrElse(displayText)

  def replacement = if (isQuoted) quote(insertText) else insertText

  private def quote(s: String) = '"' + s + '"'

}
