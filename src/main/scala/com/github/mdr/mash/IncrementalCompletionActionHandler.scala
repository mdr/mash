package com.github.mdr.mash

import com.github.mdr.mash.completions.CompletionResult

trait IncrementalCompletionActionHandler { self: Repl ⇒
  import InputAction._

  protected def handleIncrementalCompletionAction(action: InputAction, completionState: IncrementalCompletionState) {
    action match {
      case SelfInsert(c) ⇒
        state.lineBuffer = state.lineBuffer.addCharacterAtCursor(c)
        refreshCompletions(completionState)
      case BackwardDeleteChar if completionState.accepted.nonEmpty && completionState.starterPrefix.length < completionState.accepted.length ⇒
        state.lineBuffer = state.lineBuffer.backspace
        refreshCompletions(completionState)
      case Complete if completionState.immediatelyAfterCompletion ⇒ // enter browse completions mode
        state.assistanceStateOpt = None
        browseCompletion(completionState, 0)
      case _ ⇒
        state.completionStateOpt = None
        handleNormalAction(action)
    }
  }

  private def refreshCompletions(completionState: IncrementalCompletionState) {
    complete match {
      case Some(CompletionResult(completions, prefix, replacementLocation)) ⇒
        completions match {
          case Seq() ⇒
            state.completionStateOpt = None
          case Seq(completion) if prefix == completion.text ⇒
            state.completionStateOpt = None
          case _ ⇒
            val newCompletionState = IncrementalCompletionState(completionState.starterPrefix, completions, prefix, replacementLocation, immediatelyAfterCompletion = false)
            state.completionStateOpt = Some(newCompletionState)
        }
      case None ⇒
        state.completionStateOpt = None
    }
  }

}
