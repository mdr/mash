package com.github.mdr.mash.completions

import com.github.mdr.mash.utils.Region
import com.github.mdr.mash.utils.StringUtils
import com.github.mdr.mash.utils.Utils

object CompletionFragment {

  def getCommonFragment(completions: Seq[Completion], f: Completion ⇒ String): CompletionFragment =
    completions.map(getCompletionFragment(f)).reduce(_ getCommon _)

  private def getCompletionFragment(f: Completion ⇒ String)(completion: Completion): CompletionFragment = {
    val (before, after) = f(completion).splitAt(completion.location.insertPos)
    CompletionFragment(before.reverse, after)
  }

}

case class CompletionFragment(beforeReversed: String, after: String) {

  def getCommon(that: CompletionFragment): CompletionFragment = {
    val commonBefore = StringUtils.commonPrefix(this.beforeReversed, that.beforeReversed)
    val commonAfter = StringUtils.commonPrefix(this.after, that.after)
    CompletionFragment(commonBefore, commonAfter)
  }

  def before = beforeReversed.reverse

  def text = before + after

}




