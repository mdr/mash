package com.github.mdr.mash.completions

import com.github.mdr.mash.utils.Region
import com.github.mdr.mash.utils.StringUtils

/**
 * @param replacementLocation -- region of the original text to replace
 */
case class CompletionResult(completions: Seq[Completion], replacementLocation: Region) {

  assert(completions.nonEmpty)

  def sorted = copy(completions = completions.sortBy(_.displayText))

  def translate(n: Int) = copy(replacementLocation = replacementLocation.translate(n))

  def getCommonInsertText: String = {
    def common = completions.map(_.insertText).reduce(StringUtils.commonPrefix)
    if (allQuoted) quote(common) else common
  }

  private def quote(s: String) = '"' + s + '"'

  def allQuoted = completions.forall(_.isQuoted)

}
