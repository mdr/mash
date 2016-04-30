package com.github.mdr.mash.completions

import com.github.mdr.mash.utils.Region
import com.github.mdr.mash.evaluator.Environment

case class Completion(
    text: String,
    isQuoted: Boolean = false,
    completionTypeOpt: Option[CompletionType] = None,
    descriptionOpt: Option[String] = None) {

  def replacement = if (isQuoted) quote(text) else text

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
  
  def sorted = copy(completions = completions.sortBy(_.text))

  def translate(n: Int) = copy(replacementLocation = replacementLocation.translate(n))
}
