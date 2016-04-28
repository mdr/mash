package com.github.mdr.mash.repl

import com.github.mdr.mash.completions.Completion
import com.github.mdr.mash.utils.Region
import com.github.mdr.mash.utils.StringUtils

sealed trait CompletionState {

  /**
   * Region which had previously been replaced
   */
  val replacementLocation: Region

  val completions: Seq[Completion]

  def allQuoted = completions.forall(_.isQuoted)

}

/** During incremental completion, we keep a memento of the previous repl state so we can unwind */
case class ReplStateMemento(lineBuffer: LineBuffer, completionState: IncrementalCompletionState) {

  def restoreInto(replState: ReplState) {
    replState.lineBuffer = lineBuffer
    replState.completionStateOpt = Some(completionState)
  }

}

/**
 * Incremental completion state is where the user has requested completions once, but hasn't left the primary
 * line editing mode. Completions are filtered incrementally according to what the user types.
 */
case class IncrementalCompletionState(
    mementoOpt: Option[ReplStateMemento],
    completions: Seq[Completion],
    replacementLocation: Region,
    immediatelyAfterCompletion: Boolean) extends CompletionState {

  def getCommonPrefix: String = completions.map(_.text).reduce(StringUtils.commonPrefix)

  def getReplacement = if (allQuoted) "\"" + getCommonPrefix + "\"" else getCommonPrefix

}

/**
 * In the browser completion state, the user has left the primary line editing mode, and can navigate the list
 * of options using tab / the arrow keys.
 */
case class BrowserCompletionState(
    completions: Seq[Completion],
    replacementLocation: Region,
    activeCompletion: Int) extends CompletionState {

}
