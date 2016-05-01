package com.github.mdr.mash.completions

/**
 * A completion option
 * 
 * @param displayText -- text to display in the list of completions options.
 * @param insertTextOpt -- text to insert if this replacement is selected (else default to the displayText);
 *                           includes any escape characters for strings, but not quotes
 * @param isQuoted -- if true, this completion should be quoted when inserted
 */
case class Completion(
    displayText: String,
    insertTextOpt: Option[String] = None,
    isQuoted: Boolean = false,
    typeOpt: Option[CompletionType] = None,
    descriptionOpt: Option[String] = None) {

  def insertText = insertTextOpt.getOrElse(displayText)

  def replacement = if (isQuoted) quote(insertText) else insertText

  private def quote(s: String) = '"' + s + '"'

}
