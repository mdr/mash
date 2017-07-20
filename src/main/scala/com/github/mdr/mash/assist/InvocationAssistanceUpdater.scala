package com.github.mdr.mash.assist

import com.github.mdr.mash.commands.MishCommand
import com.github.mdr.mash.repl.{ LineBuffer, ReplState }
import com.github.mdr.mash.runtime.MashValue

object InvocationAssistanceUpdater {

  def toggleInvocationAssistance(state: ReplState, bindings: Map[String, MashValue]): ReplState =
    if (state.assistanceStateOpt.isDefined)
      state.copy(assistanceStateOpt = None)
    else
      updateInvocationAssistance(state, bindings)

  def updateInvocationAssistance(state: ReplState, bindings: Map[String, MashValue]): ReplState = {
    val currentAssistanceStateOpt = state.assistanceStateOpt
    val newAssistanceState = updateInvocationAssistance(state.lineBuffer, bindings, state.mish, currentAssistanceStateOpt)
    state.copy(assistanceStateOpt = newAssistanceState)
  }

  def updateInvocationAssistance(lineBuffer: LineBuffer,
                                 bindings: Map[String, MashValue],
                                 mish: Boolean,
                                 currentAssistanceStateOpt: Option[AssistanceState]): Option[AssistanceState] = {
    val text = lineBuffer.text
    val pos = lineBuffer.cursorOffset
    val newAssistanceStateOpt =
      text match {
        case MishCommand(prefix, mishCmd) ⇒
          val newPos = pos - prefix.length // adjust for the prefix
          if (newPos >= 0)
            InvocationAssistance.getCallingSyntaxOfNearestFunction(text, newPos, bindings, mish = true)
          else
            None
        case _                            ⇒
          InvocationAssistance.getCallingSyntaxOfNearestFunction(text, pos, bindings, mish = mish)
      }
    newAssistanceStateOpt orElse currentAssistanceStateOpt.filterNot(_ ⇒ text.trim.isEmpty)
  }

}
