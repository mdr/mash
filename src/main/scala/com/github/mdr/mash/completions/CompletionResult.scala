package com.github.mdr.mash.completions

import com.github.mdr.mash.utils.Region

/**
 * @param replacementLocation -- region of the original text to replace
 */
case class CompletionResult(completions: Seq[Completion], replacementLocation: Region) {

  assert(completions.nonEmpty)

  def sorted = copy(completions = completions.sortBy(_.displayText))

  def translate(n: Int) = copy(replacementLocation = replacementLocation.translate(n))
  
}
