package com.github.mdr.mash.completions

import com.github.mdr.mash.utils.Region
import com.github.mdr.mash.utils.StringUtils
import com.github.mdr.mash.utils.Utils

sealed trait CompletionContext {

  def getText(completion: Completion): String = this match {
    case CompletionContext.Insert  ⇒ completion.insertText
    case CompletionContext.Display ⇒ completion.displayText
  }

  def getPos(completion: Completion): Int = this match {
    case CompletionContext.Insert  ⇒ completion.location.insertPos
    case CompletionContext.Display ⇒ completion.location.displayPos
  }

  def getPrefixLength(completion: Completion): Int = this match {
    case CompletionContext.Insert  ⇒ completion.location.insertPrefixLength
    case CompletionContext.Display ⇒ completion.location.displayPrefixLength
  }

}

object CompletionContext {
  case object Insert extends CompletionContext
  case object Display extends CompletionContext
}

object CompletionFragment {

  def getCommonFragment(completions: Seq[Completion], completionContext: CompletionContext): CompletionFragment =
    completions.map(getCompletionFragment(completionContext)).reduce(_ getCommon _)

  private def getCompletionFragment(completionContext: CompletionContext)(completion: Completion): CompletionFragment = {
    val text = completionContext.getText(completion)
    val prefixLength = completionContext.getPrefixLength(completion)
    val (before, after) = text.drop(prefixLength).splitAt(completionContext.getPos(completion) - prefixLength)
    val prefix = text.take(prefixLength)
    CompletionFragment(prefix, before.reverse, after)
  }

}

case class CompletionFragment(prefix: String, beforeReversed: String, after: String) {

  def getCommon(that: CompletionFragment): CompletionFragment = {
    val commonBefore = StringUtils.commonPrefix(this.beforeReversed, that.beforeReversed)
    val commonAfter = StringUtils.commonPrefix(this.after, that.after)
    val newPrefix = if (this.prefix == that.prefix) this.prefix else ""
    CompletionFragment(newPrefix, commonBefore, commonAfter)
  }

  def before = beforeReversed.reverse

  def text = before + after

}




