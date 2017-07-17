package com.github.mdr.mash.repl

import com.github.mdr.mash.repl.history.History
import com.github.mdr.mash.utils.Region

/**
  * @param argIndex index into the history sequence of last arguments of the most recently inserted argument
  * @param region   region most recently inserted
  */
case class InsertLastArgState(argIndex: Int, region: Region)

object InsertLastArgHandler {

  def handleInsertLastArg(history: History, state: ReplState): ReplState = {
    val (newArgIndex, oldRegion) = state.insertLastArgStateOpt match {
      case Some(InsertLastArgState(oldArgIndex, region)) ⇒ (oldArgIndex + 1, region)
      case None                                          ⇒ (0, Region(state.lineBuffer.cursorOffset, 0))
    }
    history.getLastArg(newArgIndex) match {
      case Some(newArg) ⇒
        val newText = oldRegion.replace(state.lineBuffer.text, newArg)
        val newRegion = Region(oldRegion.offset, newArg.length)
        state.copy(
          lineBuffer = LineBuffer(newText, newRegion.posAfter),
          insertLastArgStateOpt = Some(InsertLastArgState(newArgIndex, newRegion)))
      case None         ⇒
        state
    }
  }


}
