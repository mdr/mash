package com.github.mdr.mash.assist

import com.github.mdr.mash.commands.MishCommand
import com.github.mdr.mash.repl.ReplState
import com.github.mdr.mash.runtime.MashValue

object InvocationAssistanceUpdater {

  def updateInvocationAssistance(state: ReplState, bindings: Map[String, MashValue]): ReplState = {
    val text = state.lineBuffer.text
    val pos = state.lineBuffer.cursorOffset
    val newAssistanceStateOpt =
      text match {
        case MishCommand(prefix, mishCmd) ⇒
          val newPos = pos - prefix.length // adjust for the prefix
          if (newPos >= 0)
            InvocationAssistance.getCallingSyntaxOfNearestFunction(text, newPos, bindings, mish = true)
          else
            None
        case _                            ⇒
          InvocationAssistance.getCallingSyntaxOfNearestFunction(text, pos, bindings, mish = state.mish)
      }
    state.copy(assistanceStateOpt = newAssistanceStateOpt orElse state.assistanceStateOpt.filterNot(_ ⇒ text.trim.isEmpty))
  }

}
