package com.github.mdr.mash.completions

import com.github.mdr.mash.utils.Region
import com.github.mdr.mash.utils.StringUtils
import com.github.mdr.mash.utils.Utils

object CompletionResult {

  /**
   * Merge two optional CompletionResults together, adding the set of completions together if the same
   *   replacementLocation, else preferring the first result (if available).
   */
  def merge(result1Opt: Option[CompletionResult], result2Opt: Option[CompletionResult]): Option[CompletionResult] =
    Utils.optionCombine[CompletionResult](result1Opt, result2Opt, _ merge _)

  def of(completions: Seq[Completion], replacementLocation: Region): Option[CompletionResult] =
    if (completions.isEmpty)
      None
    else
      Some(CompletionResult(completions, replacementLocation))

}

/**
 * @param replacementLocation -- region of the original text to replace
 */
case class CompletionResult(completions: Seq[Completion], replacementLocation: Region) {

  assert(completions.nonEmpty)

  def sorted = copy(completions = completions.sortBy(_.displayText))

  def translate(n: Int) = copy(replacementLocation = replacementLocation.translate(n))

  def getCommonInsertText: String = {
    val common = CompletionFragment.getCommonFragment(completions, _.insertText).text
    if (allQuoted) quote(common) else common
  }

  private def quote(s: String) = '"' + s + '"'

  def allQuoted = completions.forall(_.isQuoted)

  /**
   * Merge another CompletionResult with this one, adding its completions if and only if its replacementLocation is the same.
   * Otherwise, we just return this CompletionResult.
   */
  def merge(that: CompletionResult): CompletionResult =
    if (this.replacementLocation == that.replacementLocation)
      CompletionResult((this.completions ++ that.completions).distinct, replacementLocation)
    else
      this
}
