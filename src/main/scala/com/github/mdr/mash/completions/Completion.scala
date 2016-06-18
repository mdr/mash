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
    descriptionOpt: Option[String] = None,
    location: CompletionLocation = CompletionLocation()) {

  def insertText = insertTextOpt.getOrElse(displayText)

  def replacement = if (isQuoted) quote(insertText) else insertText

  private def quote(s: String) = '"' + s + '"'

}

/**
 * Location within the completion that corresponds to the start of the original piece of text to complete.
 *
 * For example, if the user is completing the word "123" and the completion is "foo123bar", the location would be
 * at the fourth character (pos 3 zero indexed).
 *
 * @param displayPos -- location within the displayText
 * @param insertPos -- location with the insertText
 */
case class CompletionLocation(displayPos: Int = 0, insertPos: Int = 0, displayPrefixLength: Int = 0, insertPrefixLength: Int = 0)