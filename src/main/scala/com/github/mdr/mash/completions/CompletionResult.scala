package com.github.mdr.mash.completions

import com.github.mdr.mash.utils.Region
import com.github.mdr.mash.utils.StringUtils
import com.github.mdr.mash.utils.Utils

case class CompletionFragment(beforeReversed: String, after: String) {
  def getCommon(that: CompletionFragment): CompletionFragment = {
    val commonBefore = StringUtils.commonPrefix(this.beforeReversed, that.beforeReversed)
    val commonAfter = StringUtils.commonPrefix(this.after, that.after)
    CompletionFragment(commonBefore, commonAfter)
  }
  
  def before = beforeReversed.reverse
  
  def text = before + after
  
}

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

  def getCommonText(completions: Seq[Completion], f: Completion ⇒ String): CompletionFragment =
    completions.map(getCompletionFragment(f)).reduce(_ getCommon _)

  private def getCompletionFragment(f: Completion ⇒ String)(completion: Completion): CompletionFragment = {
    val (before, after) = f(completion).splitAt(completion.location.insertPos)
    CompletionFragment(before.reverse, after)
  }

}

/**
 * @param replacementLocation -- region of the original text to replace
 */
case class CompletionResult(completions: Seq[Completion], replacementLocation: Region) {

  assert(completions.nonEmpty)

  def sorted = copy(completions = completions.sortBy(_.displayText))

  def translate(n: Int) = copy(replacementLocation = replacementLocation.translate(n))

  def getCommonInsertText: String = {
    val common = CompletionResult.getCommonText(completions, _.insertText).text
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
