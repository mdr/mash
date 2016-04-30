package com.github.mdr.mash.completions

import com.github.mdr.mash.utils.Region
import com.github.mdr.mash.evaluator.Environment

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

object CompletionType {

  val Directory = CompletionType("Directory")
  val File = CompletionType("File")
  val Flag = CompletionType("Flag")
  val Field = CompletionType("Field")
  val Method = CompletionType("Method")
  val Binding = CompletionType("Binding")
  val Function = CompletionType("Function")

}

case class CompletionType(name: String)

/**
 * @param replacementLocation -- region of the original text to replace
 */
case class CompletionResult(completions: Seq[Completion], replacementLocation: Region) {

  assert(completions.nonEmpty)

  def sorted = copy(completions = completions.sortBy(_.displayText))

  def translate(n: Int) = copy(replacementLocation = replacementLocation.translate(n))
}
